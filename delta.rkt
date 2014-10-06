#lang typed/racket/base
(provide δ)
(require racket/match racket/set
         "abbrevs.rkt" "lib.rkt" "lang.rkt" "show.rkt" "runtime.rkt" "provability.rkt")

;; Reused values
(define -Zero (-W (σ+ (+b 0)) (+b 0)))
(define -One (-W (σ+ (+b 1)) (+b 1)))

;; Adhoc macros just for use in this δ
(define-syntax-rule (requiring (clause₁ clause ... #:σ σ #:Γ Γ #:ctx ctx #:o o) body ...)
  (requiring′ σ Γ ctx o (clause₁ clause ...) body ...))
(define-syntax requiring′
  (syntax-rules (∈ ∈′)
    [(_ _ _ _ _ () body ...) (begin body ...)]
    [(_ σ Γ ctx o ([W ∈ o?] c ...) body ...)
     (match/nd : (-Ans → -Ans) (δ 'Λ σ Γ o? (list W))
       [(-Ans σ Γ (-W (-b #t) _)) (requiring′ σ Γ ctx o (c ...) body ...)]
       [(-Ans σ Γ (-W (-b #f) _)) (-Ans σ Γ (-blm ctx o (show/o o?)))])]
    [(_ σ Γ ctx o ([W ∉ o?] c ...) body ...)
     (match/nd : (-Ans → -Ans) (δ 'Λ σ Γ o? (list W))
       [(-Ans σ Γ (-W (-b #t) _))
        (-Ans σ Γ (-blm ctx o (string->symbol (format "¬~a" (show/o o?)))))]
       [(-Ans σ Γ (-W (-b #f) _)) (requiring′ σ Γ ctx o (c ...) body ...)])]))

(: δ : L -σ -Γ -o (Listof -W) → (ND -Ans))
;; Execute primitive operations
(define (δ ctx σ Γ o Ws)
  (match* (o Ws)
    ; Primitive predicates
    [{(? -pred? o) (list (and W (-W _ π)))}
     (define πₐ (π@ o (list π)))
     (define TT (-Ans σ (Γ+ Γ πₐ) (-W (-b #t) πₐ)))
     (define FF (-Ans σ (Γ+ Γ (¬π πₐ)) (-W (-b #f) πₐ)))
     (match (⊢ σ Γ W (-W (σ+ o) #|ugly|# o))
       ['✓ TT]
       ['X FF]
       ['? {set TT FF}])]
    ; Constructors
    [{(and k (-struct/cons t n)) Ws}
     (define n′ (length Ws))
     (cond
      [(= n n′)
       (define-values (αs πs) (unzip/W Ws))
       (define V (-Struct t αs))
       (define π (π@ k πs))
       (define β (σ+ V π Γ))
       (-Ans (⊔ σ β V) Γ (-W β π))]
      [else (-Ans σ Γ (-blm ctx t (format "~a requires ~a field(s), given ~a" t n n′)))])]
    ; Accessors
    [{(and acc (-struct/acc t n i)) (list (and W (-W α π)))}
     (define pred (-struct/pred t n))
     (define π₁ (π@ acc (list π)))
     (match/nd : (-V → -Ans) (σ@ σ α)
       [(-Struct t′ αs)
        (cond [(equal? t t′) (-Ans σ Γ (-W (list-ref αs i) π₁))]
              [else (-Ans σ Γ (-blm ctx (show/o acc) (show/o pred)))])]
       ['●
        (match/nd : (-Ans → -Ans) (δ ctx σ Γ pred Ws)
          [(-Ans σ₁ Γ₁ (-W (-b #t) _))
           (define β (σ+ '● π₁ Γ₁))
           (-Ans (⊔ σ₁ β '●) Γ₁ (-W β π₁))]
          [(-Ans σ₁ Γ₁ (-W (-b #f) _))
           (-Ans σ₁ Γ₁ (-blm ctx (show/o acc) (show/o pred)))])]
       [_ (-Ans σ Γ (-blm ctx (show/o acc) (show/o pred)))])]
    ; Ariths
    [((? -o/ℂℂ? o) (list (and W (-W α π))))
     (define πₐ (π@ o (list π)))
     (match/nd : (-V → -Ans) (σ@ σ α)
       [(-b (? ℂ? x))
        (define V (+b ((impl o) x)))
        (define β (σ+ V πₐ Γ))
        (-Ans (⊔ σ β V) Γ (-W β πₐ))]
       ['●
        (requiring ([W ∈ 'number?] #:σ σ #:Γ Γ #:ctx ctx #:o o)
          (define β (σ+ '● πₐ Γ))
          (-Ans (⊔ σ β '●) Γ (-W β πₐ)))]
       [_ (-Ans σ Γ (-blm ctx (show/o o) 'number?))])]
    ;; +, -, *, / are most similiar with subtle difference in #agrs and possible optimizations...
    [('+ (list Ws ...))
     (define πₐ (π@ '+ (map -W-π Ws))) ; TODO: temporarily wrong
     (for/fold : (ND -Ans) ([acc : (ND -Ans) (-Ans σ Γ -Zero)]) ([W Ws])
       ; Propagate error or check the next argument
       (match/nd : (-Ans → -Ans) acc
         [(-Ans _ _ (? -blm?)) acc]
         [(-Ans σ Γ (-W α _))
          ; Check if next argument is a number
          (match/nd : (-Ans → -Ans) (δ 'Λ σ Γ 'number? (list W))
            [(-Ans σ₁ Γ₁ (-W (-b #t) _))
             ; Update concrete result or propagate abstract number
             (match/nd : (-V → -Ans) (σ@ σ₁ α)
               [(-b (? ℂ? x))
                (match/nd : (-V → -Ans) (σ@ σ₁ W)
                  [(-b (? ℂ? y))
                   (define V (+b (+ x y)))
                   (define α (σ+ V πₐ Γ₁))
                   (-Ans (⊔ σ₁ α V) Γ₁ (-W α πₐ))]
                  ['●
                   (define α (σ+ '● πₐ Γ₁))
                   (-Ans (⊔ σ₁ α '●) Γ₁ (-W α πₐ))])]
               ['● acc])]
              [(-Ans σ₁ Γ₁ (-W (-b #f) _)) (-Ans σ₁ Γ₁ (-blm ctx '+ 'number?))])]))]
    [('- (list W₁ Ws ...))
     (define πₐ (π@ '- (map -W-π (cons W₁ Ws)))) ; TODO: temporarily wrong
     (match/nd : (-Ans → -Ans) (δ 'Λ σ Γ 'number? (list W₁))
       [(-Ans σ Γ (-W (-b #t) _))
        (for/fold : (ND -Ans) ([acc : (ND -Ans) (-Ans σ Γ W₁)]) ([W Ws])
          ; Propagate error or check the next argument
          (match/nd : (-Ans → -Ans) acc
            [(-Ans _ _ (? -blm?)) acc]
            [(-Ans σ Γ (-W α _))
             ; Check if next argument is a number
             (match/nd : (-Ans → -Ans) (δ 'Λ σ Γ 'number? (list W))
               [(-Ans σ₁ Γ₁ (-W (-b #t) _))
                ; Update concrete result or propagate abstract number
                (match/nd : (-V → -Ans) (σ@ σ₁ α)
                  [(-b (? ℂ? x))
                   (match/nd : (-V → -Ans) (σ@ σ₁ W)
                     [(-b (? ℂ? y))
                      (define V (+b (- x y)))
                      (define α (σ+ V πₐ Γ₁))
                      (-Ans (⊔ σ₁ α V) Γ₁ (-W α πₐ))]
                     ['●
                      (define α (σ+ '● πₐ Γ₁))
                      (-Ans (⊔ σ₁ α '●) Γ₁ (-W α πₐ))])]
                  ['● acc])]
               [(-Ans σ₁ Γ₁ (-W (-b #f) _)) (-Ans σ₁ Γ₁ (-blm ctx '- 'number?))])]))]
       [(-Ans σ Γ (-W (-b #f) _)) (-Ans σ Γ (-blm ctx '- 'number?))])]
    [('* (list Ws ...))
     (define πₐ (π@ '* (map -W-π Ws))) ; TODO: temporarily wrong
     (for/fold : (ND -Ans) ([acc : (ND -Ans) (-Ans σ Γ -One)]) ([W Ws])
       ; Propagate error or check the next argument
       (match/nd : (-Ans → -Ans) acc
         [(-Ans _ _ (? -blm?)) acc]
         [(-Ans σ Γ (and Wₐ (-W α _)))
          ; Check if next argument is a number
          (match/nd : (-Ans → -Ans) (δ 'Λ σ Γ 'number? (list W))
            [(-Ans σ₁ Γ₁ (-W (-b #t) _))
             ; Propagate 0 or update number
             (match (⊢ σ₁ Γ₁ Wₐ (-W 'zero? 'zero?))
               ['✓ acc]
               [_
                ; Update concrete result or propagate abstract number
                (match (⊢ σ₁ Γ₁ W (-W 'zero? 'zero?))
                  ['✓ acc]
                  [_
                   (match/nd : (-V → -Ans) (σ@ σ₁ α)
                     [(-b (? ℂ? x))
                      (match/nd : (-V → -Ans) (σ@ σ₁ W)
                        [(-b (? ℂ? y))
                         (define V (+b (* x y)))
                         (define α (σ+ V πₐ Γ₁))
                         (-Ans (⊔ σ₁ α V) Γ₁ (-W α πₐ))]
                        ['●
                         (define α (σ+ '● πₐ Γ₁))
                         (-Ans (⊔ σ₁ α '●) Γ₁ (-W α πₐ))])]
                     ['● acc])])])]
              [(-Ans σ₁ Γ₁ (-W (-b #f) _)) (-Ans σ₁ Γ₁ (-blm ctx '* 'number?))])]))]
    [('/ (list W₁ Ws ...))
     (define πₐ (π@ '- (map -W-π (cons W₁ Ws)))) ; TODO: temporarily wrong
     (match/nd : (-Ans → -Ans) (δ 'Λ σ Γ 'number? (list W₁))
       [(-Ans σ Γ (-W (-b #t) _))
        (for/fold : (ND -Ans) ([acc : (ND -Ans) (-Ans σ Γ W₁)]) ([W Ws])
          ; Propagate error or check the next argument
          (match/nd : (-Ans → -Ans) acc
            [(-Ans _ _ (? -blm?)) acc]
            [(-Ans σ Γ (and Wₐ (-W α _)))
             ; Check if next argument is a number
             (match/nd : (-Ans → -Ans) (δ 'Λ σ Γ 'number? (list W))
               [(-Ans σ₁ Γ₁ (-W (-b #t) _))
                ; Check if next argument is 0
                (match/nd : (-Ans → -Ans) (δ 'Λ σ Γ 'zero? (list W))
                  [(-Ans σ₁ Γ₁ (-W (-b #f) _))
                   ; Update concrete result or propagate abstract number
                   (match (⊢ σ₁ Γ₁ Wₐ (-W 'zero? 'zero?))
                     ['✓ (-Ans σ₁ Γ₁ -Zero)]
                     [_
                      (match/nd : (-V → -Ans) (σ@ σ₁ α)
                        [(-b (? ℂ? x))
                         (match/nd : (-V → -Ans) (σ@ σ₁ W)
                           [(-b (? ℂ? y))
                            (define V (+b (/ x y)))
                            (define α (σ+ V πₐ Γ₁))
                            (-Ans (⊔ σ₁ α V) Γ₁ (-W α πₐ #|temp. wrong|#))]
                           ['●
                            (define α (σ+ '● πₐ Γ₁))
                            (-Ans (⊔ σ₁ α '●) Γ₁ (-W α πₐ))])]
                        ['● acc])])]
                  [(-Ans σ₁ Γ₁ (-W (-b #t) _)) (-Ans σ₁ Γ₁ (-blm ctx '/ '≠0))])]
               [(-Ans σ₁ Γ₁ (-W (-b #f) _)) (-Ans σ₁ Γ₁ (-blm ctx '/ 'number?))])]))]
       [(-Ans σ Γ (-W (-b #f) _)) (-Ans σ Γ (-blm ctx '/ 'number?))])]
    [('string-length (list (and W (-W α π))))
     (define πₐ (π@ 'string-length (list π)))
     (match/nd : (-V → -Ans) (σ@ σ α)
       [(-b (? string? s))
        (define V (+b (string-length s)))
        (define β (σ+ V πₐ Γ))
        (-Ans (⊔ σ α V) Γ (-W β πₐ))]
       ['●
        (match/nd : (-Ans → -Ans) (δ 'Λ σ Γ 'string? Ws)
          [(-Ans σ₁ Γ₁ (-W (-b #t) _))
           (define β (σ+ '● πₐ Γ))
           (-Ans (⊔ σ₁ β '●) Γ₁ (-W β πₐ))]
          [(-Ans σ₁ Γ₁ (-W (-b #f) _)) (-Ans σ₁ Γ₁ (-blm ctx 'string-length 'string?))])]
       [_ (-Ans σ Γ (-blm ctx 'string-length 'string?))])]
    [('vector-length (list (and W (-W α π))))
     (define πₐ (π@ 'vector-length (list π)))
     (match/nd : (-V → -Ans) (σ@ σ α)
       [(-Vec/Raw vs)
        (define V (+b (length vs)))
        (define β (σ+ V πₐ Γ))
        (-Ans (⊔ σ β V) Γ (-W β πₐ))]
       [(-Vec/Wrapped _ γ _)
        (match/nd : (-V → -Ans) (σ@ σ γ)
          [(-Vec/C γs)
           (define V (+b (length γs)))
           (define β (σ+ V πₐ Γ))
           (-Ans (⊔ σ β V) Γ (-W β πₐ))]
          [_ ; ok by construction
           (define β (σ+ '● πₐ Γ))
           (-Ans (⊔ σ β '●) Γ (-W β πₐ))])]
       ['●
        (match/nd : (-Ans → -Ans) (δ 'Λ σ Γ 'vector? Ws)
          [(-Ans σ Γ (-W (-b #t) _))
           (define β (σ+ '● πₐ Γ))
           (-Ans (⊔ σ β '●) Γ (-W β πₐ))]
          [(-Ans σ Γ (-W (-b #f) _)) (-Ans σ Γ (-blm ctx 'vector-length 'vector?))])]
       [_ (-Ans σ Γ (-blm ctx 'vector-length 'vector?))])]
    [('procedure-arity (list (and W (-W α π))))
     (define πₐ (π@ 'procedure-arity (list π)))
     (match/nd : (-V → -Ans) (σ@ σ α)
       [(? -o¹?) (-Ans σ Γ -One)]
       [(? -o²?)
        (define V (+b 2))
        (define β (σ+ V πₐ Γ))
        (-Ans (⊔ σ β V) Γ (-W β πₐ))]
       [(? -o³?)
        (define V (+b 3))
        (define β (σ+ V πₐ Γ))
        (-Ans (⊔ σ β V) Γ (-W β πₐ))]
       [(-struct/cons _ n)
        (define V (+b n))
        (define β (σ+ V πₐ Γ))
        (-Ans (⊔ σ β V) Γ (-W β πₐ))]
       [(-Λ xs _ _ _)
        (define V (+b (length xs)))
        (define β (σ+ V πₐ Γ))
        (-Ans (⊔ σ β V) Γ (-W β πₐ))]
       [(-Arr _ γ _)
        (match/nd : (-V → -Ans) (σ@ σ γ)
          [(-Λ/C dom _ _ _)
           (define V (+b (length dom)))
           (define β (σ+ V πₐ Γ))
           (-Ans (⊔ σ β V) Γ (-W β πₐ))]
          [_ (error "This can't happen. Must be a function contract.")])]
       ['●
        (match/nd : (-Ans → -Ans) (δ 'Λ σ Γ 'procedure? Ws)
          [(-Ans σ₁ Γ₁ (-W (-b #t) _))
           (define β (σ+ '● πₐ Γ₁))
           (-Ans (⊔ σ₁ β '●) Γ₁ (-W β πₐ))]
          [(-Ans σ₁ Γ₁ (-W (-b #f) _)) (-Ans σ₁ Γ₁ (-blm ctx 'procedure-arity 'procedure?))])]
       [_ (-Ans σ Γ (-blm ctx 'procedure-arity 'procedure?))])]
    [('sqrt (list (and W (-W α π))))
     (define πₐ (π@ 'sqrt (list π)))
     (match/nd : (-V → -Ans) (σ@ σ α)
       [(-b (? ℂ? x))
        (define V (+b (sqrt x)))
        (define β (σ+ V πₐ Γ))
        (-Ans (⊔ σ β V) Γ (-W β πₐ))]
       ['●
        (match/nd : (-Ans → -Ans) (δ ctx σ Γ 'number? (list W))
          [(-Ans σ₁ Γ₁ (-W (-b #t) _))
           (define β (σ+ '● πₐ Γ₁))
           (-Ans (⊔ σ₁ β '●) Γ₁ (-W β πₐ))]
          [(-Ans σ₁ Γ₁ (-W (-b #f) _))
           (-Ans σ₁ Γ₁ (-blm ctx 'sqrt 'number?))])]
       [_ (-Ans σ Γ (-blm ctx 'sqrt 'number?))])]
    ; Comparison
    [('equal? (list (-W α₁ π₁) (-W α₂ π₂)))
     (define πₐ (π@ 'equal? (list π₁ π₂)))
     (define TT (-Ans σ (Γ+ Γ πₐ) (-W (-b #t) πₐ)))
     (define FF (-Ans σ (Γ+ Γ (¬π πₐ)) (-W (-b #f) πₐ)))
     (match (Γ⊢ Γ πₐ)
       ['✓ TT]
       ['X FF]
       ['?
        (match/nd : (-V → -Ans) (σ@ σ α₁)
          [(-b b₁) (match/nd : (-V → -Ans) (σ@ σ α₂)
                     [(-b b₂) (if (equal? b₁ b₂) TT FF)]
                     [_ {set TT FF}])]
          [_ {set TT FF}])])]
    [('= (list (and W₁ (-W α₁ π₁)) (and W₂ (-W α₂ π₂))))
     (define πₐ (π@ '= (list π₁ π₂)))
     (match*/nd : (-V -V → -Ans) ((σ@ σ α₁) (σ@ σ α₂))
       [((-b (? ℂ? x)) (-b (? ℂ? y)))
        (-Ans σ Γ (-W (+b (= x y)) πₐ))]
       [(_ _)
        (requiring ([W₁ ∈ 'number?]
                    [W₂ ∈ 'number?]
                    #:σ σ #:Γ Γ #:ctx ctx #:o '=)
          (define TT (-Ans σ (Γ+ Γ πₐ) (-W (+b #t) πₐ)))
          (define FF (-Ans σ (Γ+ Γ (¬π πₐ)) (-W (+b #f) πₐ)))
          (match (Γ⊢ Γ πₐ)
            ['✓ TT]
            ['X FF]
            ['? {set TT FF}]))])]
    [((? -o/ℝℝ𝔹? o) (list (and W₁ (-W α₁ π₁)) (and W₂ (-W α₂ π₂))))
     (define πₐ (π@ o (list π₁ π₂)))
     (match*/nd : (-V -V → -Ans) ((σ@ σ α₁) (σ@ σ α₂))
       [((-b (? ℝ? x)) (-b (? ℝ? y))) (-Ans σ Γ (-W (+b ((impl o) x y)) πₐ))]
       [(_ _)
        (requiring ([W₁ ∈ 'real?]
                    [W₂ ∈ 'real?]
                    #:σ σ #:Γ Γ #:ctx ctx #:o o)
          (define TT (-Ans σ (Γ+ Γ πₐ) (-W (+b #t) πₐ)))
          (define FF (-Ans σ (Γ+ Γ (¬π πₐ)) (-W (+b #f) πₐ)))
          (match (Γ⊢ Γ πₐ)
            ['✓ TT]
            ['X FF]
            ['? {set TT FF}]))])]
    [(_ _) (errorf "Invalid operation: ~a on ~a" (show/o o) Ws)]))

(: impl : (case->
           [-o/ℝℝ𝔹 → (ℝ ℝ → 𝔹)]
           [-o/ℂℂ → (ℂ → ℂ)]))
(define impl (match-lambda ['> >] ['< <] ['≥ >=] ['≤ <=]
                      ['add1 add1] ['sub1 sub1]))
