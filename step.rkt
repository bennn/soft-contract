#lang typed/racket/base
(provide (all-defined-out))
(require racket/match racket/list racket/set
         "abbrevs.rkt" "lib.rkt" "lang.rkt" "runtime.rkt" "delta.rkt" "show.rkt")

(define -Void (-Struct 'void '()))

(define (inj [p : -p])
  (match-define (cons m₁ p′) p)
  (match-define-values (m₁↓ (cons (Cmd b e) cmds)) (m→cmds m₁))
  (-Σ/kont p↓∅ p′ ; PP
             m₁↓ cmds b ; MMB
             (-ς/kont (-↓ e ρ∅) Γ∅ σ₀ '()))) ; CEΓSK

;; Step program state
(: ↦ₚ : -Σ/kont → (ND -Σ))
(define (↦ₚ Σ)
  (match-define (-Σ/kont p↓ p m↓ m b ς) Σ)
  (match/nd : (-ς → -Σ) (↦ p↓ m↓ ς)
    [(? -ς/kont? ς′) #;(printf "~a~n~n" ς′) (-Σ/kont p↓ p m↓ m b ς′)]
    [(-Ans σ _ (? -blm? blm)) (-Σ/err p↓ σ b blm)]
    [(-Ans σ Γ (-W α _))
     (define m↓′ (m+ m↓ b α))
     (match m
       [(cons (Cmd b′ e) cmds) ; Continue with current module
        (-Σ/kont p↓ p m↓′ cmds b′ (-ς/kont (-↓ e ρ∅) Γ∅ σ '()))]
       ['() ; Done with this module
        (define p↓′ (hash-set p↓ (-m↓-name m↓′) m↓′))
        (match p
          [(cons m₁ p′) ; Continue to next module
           (match-define-values (m₁↓ (cons (Cmd b′ e) cmds)) (m→cmds m₁))
           (-Σ/kont p↓′ p′ m₁↓ cmds b′ (-ς/kont (-↓ e ρ∅) Γ∅ σ '()))]
          ['() ; Done program
           (-Σ/ok p↓′ σ)])])]))

;; Step expression state (CESK + Γ)
(: ↦ : -p↓ -m↓ -ς/kont → (ND -ς))
(define (↦ p↓ m↓ ς)
  (match-define (-ς/kont E Γ σ κ) ς)
  
  ;; Lookup defined definition/contract
  (: m@ : L Symbol L → (U (Pairof -α -α) -α))
  (define (m@ ctx name in)
    (match (hash-ref p↓ in (λ () m↓))
      [(-m↓ (? (λ (l) (equal? l in)) l) provs _ defs)
       (cond [(equal? ctx in) (hash-ref defs name)]
             [else (cons (hash-ref provs name) (hash-ref defs name))])]
      [_ (errorf "Module `~a` not available during execution" in)]))
  
  ;; 'Push' states
  (: ↦e : -e -ρ → (ND -ς))
  (define (↦e e ρ)
    (match e
      [(? -v? v)
       (define-values (V π) (close v ρ Γ))
       (define α (σ+ V π Γ))
       (-ς/kont (-W α π) Γ (⊔ σ α V) κ)]
      [(-x x) (-ς/kont (hash-ref ρ x) Γ σ κ)]
      [(-ref ctx name in)
       (match (m@ ctx name in)
         [(cons γ α) (-ς/kont (-W α 'ø) Γ σ (cons (-monᶜₖ (list in ctx in) (-W γ 'ø)) κ) )]
         [(? -α? α) (-ς/kont (-W α 'ø) Γ σ κ)])]
      [(-@ ctx f xs)
       (-ς/kont (-↓ f ρ) Γ σ (cons (-@ₖ ctx (for/list ([x xs]) (-↓ x ρ)) '()) κ))]
      [(-@/havoc _) (error "TODO: havoc")]
      [(-if i t e)
       (-ς/kont (-↓ i ρ) Γ σ (cons (-ifₖ (-↓ t ρ) (-↓ e ρ)) κ))]
      [(-let '() e) (-ς/kont (-↓ e ρ) Γ σ κ)]
      [(-let (cons (cons x e₁) xes) e)
       (-ς/kont (-↓ e₁ ρ) Γ σ (cons (-letₖ x xes '() e ρ) κ))]
      [(-amb es) (for/setof: -ς/kont ([e es])
                   (-ς/kont (-↓ e ρ) Γ σ κ))]
      [(-λ/c xcs d)
       (match xcs
         ['()
          (define C (-Λ/C '() d ρ Γ))
          (define α (σ+ C 'ø Γ))
          (-ς/kont (-W α 'ø) Γ (⊔ σ α C) κ)]
         [(cons (cons x c) xcs)
          (-ς/kont (-↓ c ρ) Γ σ (cons (-λ/cₖ x xcs '() d ρ) κ))])]
      [(-struct/c t cs)
       (match cs
         ['()
          (define C (-Struct/C t '()))
          (define α (σ+ C 'ø Γ))
          (-ς/kont (-W α 'ø) Γ (⊔ σ α C) κ)]
         [(cons c cs)
          (-ς/kont (-↓ c ρ) Γ σ (cons (-struct/cₖ t cs ρ '()) κ))])]
      ; Box stuff
      [(-box e) (-ς/kont (-↓ e ρ) Γ σ (cons 'box κ))]
      [(-unbox l e) (-ς/kont (-↓ e ρ) Γ σ (cons (-unboxₖ l) κ))]
      [(-set-box! l eₓ eᵥ) (-ς/kont (-↓ eₓ ρ) Γ σ (cons (-set-box!ₖ₁ l eᵥ ρ) κ))]
      ; Vector stuff
      [(-vector es)
       (match es
         ['()
          (define V (-Vec/Raw '()))
          (define α (σ+ V))
          (-ς/kont (-W α 'ø) Γ (⊔ σ α V) κ)]
         [(cons e₁ eᵣ) (-ς/kont (-↓ e₁ ρ) Γ σ (cons (-vectorₖ eᵣ ρ '()) κ))])]
      [(-vector-ref l e eᵢ) (-ς/kont (-↓ e ρ) Γ σ (cons (-vector-refₖ₁ l eᵢ ρ) κ))]
      [(-vector-set! l e eᵢ eᵥ) (-ς/kont (-↓ e ρ) Γ σ (cons (-vector-set!ₖ₁ l eᵢ eᵥ ρ) κ))]))
  
  ;; 'Pop' states
  (: ↦W : -W → (ND -ς))
  (define (↦W W)
    (match κ
      ['() (-Ans σ Γ W)]
      [(cons Φ κ′) (↦Φ W Φ κ′)]))
  
  (: ↦Φ : -W -Φ -κ → (ND -ς))
  (define (↦Φ W Φ κ)
    (match Φ
      [(-ifₖ T E)
       (match/nd : (-Ans → -ς) (δ 'Λ σ Γ 'false? (list W))
         [(-Ans σ′ Γ′ (-W (-b #f) _)) (-ς/kont T Γ′ σ′ κ)]
         [(-Ans σ′ Γ′ (-W (-b #t) _)) (-ς/kont E Γ′ σ κ)])]
      [(-@ₖ ctx Es Ws)
       (match Es
         ['()
          (match-define (cons f xs) (reverse (cons W Ws)))
          (↦@ ctx f xs κ)]
         [(cons E₁ Eᵣ) (-ς/kont E₁ Γ σ (cons (-@ₖ ctx Eᵣ (cons W Ws)) κ))])]
      [(-letₖ x es Ws e ρ)
       (match es ; FIXME add rt frame
         ['()
          (-ς/kont (-↓ e (ρ+ ρ (cons (cons x (W+ W (+x x))) Ws)))
                   Γ
                   σ
                   (cons (-rt/letₖ (FV/Γ Γ)) κ))]
         [(cons (cons z e₁) eᵣ)
          (-ς/kont (-↓ e₁ ρ) Γ σ (cons (-letₖ z eᵣ (cons (cons x (W+ W (+x x))) Ws) e ρ) κ))])]
      ; Return stuff
      [(-rtₖ Γ₀ bs)
       (match-define (-W α π) W)
       (define-values (x* π*) (unzip bs))
       (define xs (list->set (map -x-name x*)))
       (define π′ (for/fold ([π : -π′ (restrict/π π xs)]) ([xᵢ x*] [πᵢ π*])
                    (π/ π xᵢ πᵢ)))
       (define Γ′ (for/fold ([Γ : -Γ (restrict/Γ Γ xs)]) ([xᵢ x*] [πᵢ π*])
                    (Γ/ Γ xᵢ πᵢ)))
       (-ς/kont (-W α π′) (∪ Γ₀ Γ′) σ κ)]
      [(-rt/letₖ xs)
       (match-define (-W α π) W)
       (-ς/kont (-W α (restrict/π π xs)) (restrict/Γ Γ xs) σ κ)]
      ; Contract creation stuff
      [(-struct/cₖ t '() ρ (list W₁ ...))
       (define Ws (reverse (cons W W₁)))
       (define-values (αs πs) (unzip/W Ws))
       (define V (-Struct/C t αs))
       (define α (σ+ V (π@ (-struct/cons t (length Ws)) πs) Γ))
       (-ς/kont (-W α 'ø) Γ (⊔ σ α V) κ)]
      [(-struct/cₖ t (cons e₁ es) ρ Ws)
       (-ς/kont (-↓ e₁ ρ) Γ σ (cons (-struct/cₖ t es ρ (cons W Ws)) κ))]
      [(-λ/cₖ x '() xCs d ρ)
       (define V (-Λ/C (reverse (cons (cons x (-W-α W)) xCs)) d ρ Γ))
       (define α (σ+ V 'ø Γ))
       (-ς/kont (-W α 'ø) Γ (⊔ σ α V) κ)]
      [(-λ/cₖ x (cons (cons z c) xcs) xCs d ρ)
       (-ς/kont (-↓ c ρ) Γ σ (cons (-λ/cₖ z xcs (cons (cons x (-W-α W)) xCs) d ρ) κ))]
      ; Box stuff
      ['box
       (define V (-Box/Raw (-W-α W)))
       (define α (σ+ V 'ø Γ))
       (-ς/kont (-W α 'ø) Γ (⊔ σ α V) κ)]
      [(-unboxₖ l)
       (match/nd : (-V → -ς) (σ@ σ W)
         [(-Box/Raw β) (-ς/kont (-W β 'ø) Γ σ κ)]
         [(-Box/Wrapped l³ γ β) (error "TODO: unbox/wrapped")]
         ['● (match/nd : (-Ans → -ς) (δ l σ Γ 'box? (list W))
               [(-Ans σ Γ (-W (-b #t) _))
                (define β (σ+ '● 'ø Γ))
                (-ς/kont (-W β 'ø) Γ (⊔ σ β '●) κ)]
               [(-Ans σ Γ (-W (-b #f) _)) (-Ans σ Γ (-blm l 'unbox 'box?))])]
         [_ (-Ans σ Γ (-blm l 'unbox 'box?))])]
      [(-set-box!ₖ₁ l e ρ) (-ς/kont (-↓ e ρ) Γ σ (cons (-set-box!ₖ₂ l (-W-α W)) κ))]
      [(-set-box!ₖ₂ l α)
       (define V (-Struct 'void '()))
       (define αᵣ (σ+ V))
       (match/nd : (-V → -ς) (σ@ σ α)
         [(-Box/Raw _) (-ς/kont (-W αᵣ 'ø) Γ (⊔ (⊔ σ αᵣ V) α (-Box/Raw (-W-α W))) κ)]
         [(-Box/Wrapped l³ γ β) (error "TODO: set-box!/wrapped")]
         ['● (match/nd : (-Ans → -ς) (δ l σ Γ 'box? (list W))
               [(-Ans σ Γ (-W (-b #t) _)) (-ς/kont (-W αᵣ 'ø) Γ (⊔ σ αᵣ V) κ)]
               [(-Ans σ Γ (-W (-b #f) _)) (-Ans σ Γ (-blm l 'set-box! 'box?))])])]
      ; Vector stuff
      [(-vectorₖ es ρ αs)
       (match es
         ['()
          (define V (-Vec/Raw (reverse (cons (-W-α W) αs))))
          (define α (σ+ V 'ø Γ))
          (-ς/kont (-W α 'ø) Γ (⊔ σ α V) κ)]
         [(cons e₁ eᵣ)
          (-ς/kont (-↓ e₁ ρ) Γ σ (cons (-vectorₖ eᵣ ρ (cons (-W-α W) αs)) κ))])]
      [(-vector-refₖ₁ l eᵢ ρ)
       (-ς/kont (-↓ eᵢ ρ) Γ σ (cons (-vector-refₖ₂ l W) κ))]
      [(-vector-refₖ₂ l Wₑ)
       (match*/nd : (-V -V → -ς) ((σ@ σ Wₑ) (σ@ σ W))
         [((-Vec/Raw αs) (-b (? ℕ? i)))
          (cond [(and (<= 0 i) (< i (length αs))) (-ς/kont (-W (list-ref αs i) 'ø) Γ σ κ)]
                [else (-Ans σ Γ (-blm l 'vector-ref 'bound))])]
         [((-Vec/Wrapped l³ γ α) (-b (? integer? i)))
          (error "TODO: vector-ref/wrapped")]
         [(Vₑ Vᵢ)
          (match/nd : (-Ans → -ς) (δ 'Λ σ Γ 'vector? (list Wₑ))
            [(-Ans σ Γ (-W (-b #t) _))
             (match/nd : (-Ans → -ς) (δ 'Λ σ Γ 'integer? (list W))
               [(-Ans σ Γ (-W (-b #t) _))
                (match/nd : (-Ans → -ς) (δ 'Λ σ Γ '≤ (list (-W (+b 0) (+b 0)) W))
                  [(-Ans σ Γ (-W (-b #t) _))
                   (match/nd : (-Ans → -ς) (δ 'Λ σ Γ 'vector-length (list Wₑ))
                     [(-Ans σ Γ (? -W? Wₗ))
                      (match/nd : (-Ans → -ς) (δ 'Λ σ Γ '< (list W Wₗ))
                        [(-Ans σ Γ (-W (-b #t) _))
                         (define αᵣ (σ+ '● 'ø Γ))
                         (-ς/kont (-W αᵣ 'ø) Γ σ κ)]
                        [(-Ans σ Γ (-W (-b #f) _))
                         (-Ans σ Γ (-blm l 'vector-ref '(< length)))]
                        [_ ∅ #|ignore ok|#])]
                     [_ ∅ #|ignore ok|#])]
                  [(-Ans σ Γ (-W (-b #f) _)) (-Ans σ Γ (-blm l 'vector-ref '(≥ 0)))])]
               [(-Ans σ Γ (-W (-b #f) _)) (-Ans σ Γ (-blm l 'vector-ref 'integer?))])]
            [(-Ans σ Γ (-W (-b #f) _)) (-Ans σ Γ (-blm l 'vector-ref 'vector?))])])]
      [(-vector-set!ₖ₁ l eᵢ eᵥ ρ)
       (-ς/kont (-↓ eᵢ ρ) Γ σ (cons (-vector-set!ₖ₂ l W eᵥ ρ) κ))]
      [(-vector-set!ₖ₂ l Wᵥ eᵥ ρ)
       (-ς/kont (-↓ eᵥ ρ) Γ σ (cons (-vector-set!ₖ₃ l Wᵥ W) κ))]
      [(-vector-set!ₖ₃ l Wᵥ Wᵢ)
       (define αᵤ (-W-α W))
       (define Void (-Struct 'void '()))
       (define β (σ+ Void))
       (define αᵥ (-W-α Wᵥ))
       (match*/nd : (-V -V → -ς) ((σ@ σ Wᵥ) (σ@ σ Wᵢ))
         [((-Vec/Raw αs) (-b (? ℕ? i)))
          (cond [(and (<= 0 i) (< i (length αs)))
                 (-ς/kont (-W β 'ø) Γ (⊔ (⊔ σ αᵥ (-Vec/Raw (list-set αs i αᵤ))) β Void) κ)]
                [else (-Ans σ Γ (-blm l 'vector-set! 'bound))])]
         [((-Vec/Wrapped l³ γ α) (-b (? ℕ? i)))
          (error "TODO: vector-set!/wrapped")]
         [(Vᵥ Vᵢ)
          (match/nd : (-Ans → -ς) (δ 'Λ σ Γ 'vector? (list Wᵥ))
            [(-Ans σ Γ (-W (-b #t) _))
             (match/nd : (-Ans → -ς) (δ 'Λ σ Γ 'integer? (list Wᵢ))
               [(-Ans σ Γ (-W (-b #t) _))
                (match/nd : (-Ans → -ς) (δ 'Λ σ Γ '≤ (list (-W (+b 0) (+b 0)) W))
                  [(-Ans σ Γ (-W (-b #t) _))
                   (match/nd : (-Ans → -ς) (δ 'Λ σ Γ 'vector-length (list Wᵥ))
                     [(-Ans σ Γ (? -W? Wₗ))
                      (match/nd : (-Ans → -ς) (δ 'Λ σ Γ '< (list Wᵢ Wₗ))
                        [(-Ans σ Γ (-W (-b #t) _))
                         (define α• (σ+ '● 'ø Γ))
                         (define V (match Wₗ
                                     [(-W (-b (? ℕ? n)) _) (-Vec/Raw (make-list n α•))]
                                     [_ '●]))
                         (-ς/kont (-W β 'ø) Γ (⊔ (⊔ σ αᵥ V) β Void) κ)]
                        [(-Ans σ Γ (-W (-b #f) _))
                         (-Ans σ Γ (-blm l 'vector-set! '(< length)))]
                        [_ ∅ #|ignore ok|#])]
                     [_ ∅ #|ignore ok|#])]
                  [(-Ans σ Γ (-W (-b #f) _)) (-Ans σ Γ (-blm l 'vector-set! '(≥ 0)))])]
               [(-Ans σ Γ (-W (-b #f) _)) (-Ans σ Γ (-blm l 'vector-set! 'integer?))])]
            [(-Ans σ Γ (-W (-b #f) _)) (-Ans σ Γ (-blm l 'vector-set! 'vector?))])])]
      ; Monitoring stuff
      [(-monᶜₖ l³ E) (-ς/kont E Γ σ (cons (-monᵛₖ l³ W) κ))]
      [(-monᵛₖ l³ C) (↦C l³ C W Γ σ κ)]
      [(-FC l γ W)
       (match/nd : (-V → -ς) (σ@ σ γ)
         [(-Struct 'and/c (list γ₁ γ₂))
          (-ς/kont (-FC l γ₁ W) Γ σ (cons (-ifₖ (-FC l γ₂ W) (-W (+b #f) (+b #f))) κ))]
         [(-Struct 'or/c (list γ₁ γ₂))
          (-ς/kont (-FC l γ₁ W) Γ σ (cons (-ifₖ (-W (+b #t) (+b #t)) (-FC l γ₂ W)) κ))]
         [(-Struct 'not/c (list γ′))
          (-ς/kont (-FC l γ′ W) Γ σ (cons (-ifₖ (-W (+b #f) (+b #f)) (-W (+b #t) (+b #t))) κ))]
         [_ (-ς/kont W Γ σ (cons (-@ₖ l '() (list (-W γ 'ø))) κ))])]))
  
  (: ↦C : L³ -W -W -Γ -σ -κ → (ND -ς))
  (define (↦C l³ C W Γ σ κ)
    ; FIXME inconsistent with paper for now
    (match-define (list l₊ _ lₒ) l³)
    (match/nd : (-V → -ς) (σ@ σ C)
      [(-Λ/C xcs d ρ Γ)
       (match/nd : (-Ans → -ς) (δ 'Λ σ Γ 'procedure? (list W))
         [(-Ans σ Γ (-W (-b #t) _))
          (match/nd : (-Ans → -ς) (δ 'Λ σ Γ 'procedure-arity (list W))
            [(-Ans σ Γ (? -W? Wᵣ))
             (define Vₗ (+b (length xcs)))
             (define αₗ (σ+ Vₗ))
             (match/nd : (-Ans → -ς) (δ 'Λ (⊔ σ αₗ Vₗ) Γ '= (list (-W αₗ Vₗ) Wᵣ))
               [(-Ans σ Γ (-W (-b #t) _))
                (define V (-Arr l³ (-W-α C) (-W-α W)))
                (define αᵣ (σ+ V (-W-π W) Γ))
                (-ς/kont (-W αᵣ 'ø) Γ (⊔ σ αᵣ V) κ)]
               [(-Ans σ Γ (-W (-b #f) _)) (-Ans σ Γ (-blm l₊ lₒ `(arity ,(length xcs))))])]
            [(-Ans σ Γ (? -blm?)) ∅ #|ignore ok|#])]
         [(-Ans σ Γ (-W (-b #f) _)) (-Ans σ Γ (-blm l₊ lₒ 'procedure?))])]
      [(? -Box/C?)
       (match/nd : (-Ans → -ς) (δ 'Λ σ Γ 'box? (list W))
         [(-Ans σ Γ (-W (-b #t) _))
          (define V (-Box/Wrapped l³ (-W-α C) (-W-α W)))
          (define αᵣ (σ+ V (-W-π W) Γ))
          (-ς/kont (-W αᵣ 'ø) Γ (⊔ σ αᵣ V) κ)]
         [(-Ans σ Γ (-W (-b #f) _)) (-Ans σ Γ (-blm l₊ lₒ 'box?))])]
      [(-Vec/C cs)
       (match/nd : (-Ans → -ς) (δ 'Λ σ Γ 'vector? (list W))
         [(-Ans σ Γ (-W (-b #t) _))
          (match/nd : (-Ans → -ς) (δ 'Λ σ Γ 'vector-length (list W))
            [(-Ans σ Γ (? -W? Wᵣ))
             (define Vₗ (+b (length cs)))
             (define αₗ (σ+ Vₗ))
             (match/nd : (-Ans → -ς) (δ 'Λ (⊔ σ αₗ Vₗ) Γ '= (list (-W αₗ Vₗ) Wᵣ))
               [(-Ans σ Γ (-W (-b #t) _))
                (define V (-Vec/Wrapped l³ (-W-α C) (-W-α W)))
                (define αᵣ (σ+ V (-W-π W) Γ))
                (-ς/kont (-W αᵣ 'ø) Γ (⊔ σ αᵣ V) κ)]
               [(-Ans σ Γ (-W (-b #f) _)) (-Ans σ Γ (-blm l₊ lₒ `(vector-length ,(length cs))))])]
            [(-Ans σ Γ (? -blm?)) ∅ #|ignore ok|#])]
         [(-Ans σ Γ (-W (-b #f) _)) (-Ans σ Γ (-blm l₊ lₒ 'vector?))])]
      [(-Struct 'and/c (list γ₁ γ₂))
       (-ς/kont W Γ σ (list* (-monᵛₖ l³ (-W γ₁ 'ø)) (-monᵛₖ l³ (-W γ₂ 'ø)) κ))]
      [(-Struct 'or/c (list γ₁ γ₂))
       (-ς/kont (-FC lₒ γ₁ W) Γ σ (cons (-ifₖ W (-Mon l³ γ₂ (-W-α W))) κ))]
      [(-Struct 'not/c (list γ))
       (-ς/kont (-FC lₒ γ W) Γ σ (cons (-ifₖ (-blm l₊ lₒ C) W) κ))]
      #;[(-Struct t γs)
       (define n (length γs))
       (define pred (-struct/pred t n))
       (match/nd : (-Ans → -ς) (δ 'Λ pred σ Γ (list W))
         [(-Ans σ Γ (-W (-b #t) _))
          (match γs
            ['() (-ς/kont W Γ σ κ)]
            [(cons γ γs)
             ])]
         [(-Ans σ Γ (-W (-b #f) _)) (-Ans σ Γ (-blm l₊ lₒ (show/o pred)))])]
      ; TODO: assume no opaque contract for now
      [_ (error "TODO")]))
    
  (: ↦@ : L -W (Listof -W) -κ → (ND -ς))
  (define (↦@ ctx F Xs κ)
    (match-define (-W α π) F)
    (match/nd : (-V → -ς) (σ@ σ α)
      [(? -o? o) (↦δ ctx o Xs κ)]
      [(-Λ xs e ρ Γf) (↦λ ctx xs e ρ Γf Xs κ)]
      [(-Arr l³ γ α)
       ]))
  
  (: ↦δ : L -o (Listof -W) -κ → (ND -ς))
  (define (↦δ ctx o Ws κ)
    (match/nd : (-Ans → -ς) (δ ctx σ Γ o Ws)
      [(-Ans σ′ Γ′ A) (-ς/kont A Γ′ σ′ κ)]))
  
  (: ↦λ : L (Listof Symbol) -e -ρ -Γ (Listof -W) -κ → (ND -ς))
  (define (↦λ ctx xs e ρ Γf Ws κ)
    (-ς/kont (-↓ e (ρ+ ρ xs (for/list : (Listof -W) ([W Ws] [x xs]) (-W (-W-α W) (+x x)))))
             Γf
             σ
             (cons (-rtₖ Γ (for/list ([x xs] [π (map -W-π Ws)] #:when (-π? π))
                             (cons (+x x) π)))
                   κ)))
  
  (match E
    [(-↓ e ρ) (↦e e ρ)]
    [(? -W? W) (↦W W)]
    [(? -blm? blm) (-Ans σ Γ blm)]))
