#lang typed/racket/base
(provide Derivability ⊢ σ⊢ Γ⊢ Γ⊢ₓ Γ⊢ᵢ)
(require racket/match racket/set racket/function "abbrevs.rkt" "lib.rkt" "lang.rkt" "runtime.rkt")

(define-type Derivability (U '✓ 'X '?))

(: ⊢ : -σ -Γ -W -W → Derivability)
(define (⊢ σ Γ X P)
  ;(printf "⊢ ~a ~a ~a ~a~n" σ Γ P X)
  (match-let ([(-W α πₓ) X]
              [(-W γ πₚ) P])
    (match (Γ⊢ Γ (π@ πₚ (list πₓ)))
      ['? (σ⊢ σ α γ)]
      [d d])))

(: σ⊢ : -σ -α -α → Derivability)
(define (σ⊢ σ α γ) ; TODO
  (let ([Cs (hash-ref σ γ)]
        [Vs (hash-ref σ α)])
    (or (for*/fold : (Option Derivability) ([r : (Option Derivability) #f]) ([V Vs] [C Cs])
          (match r
            ['? '?]
            [r (match* (r (V⊢ σ V C)) ; TODO 2d
                 [(#f r′) r′]
                 [('✓ '✓) '✓]
                 [('X 'X) 'X]
                 [(_ _) '?])]))
        '✓)))

(: V⊢ : -σ -V -V → Derivability)
;; Check if value definitely prove/refute flat contract
(define (V⊢ σ V C)
  (match* (V C)
    ; Base cases
    [((-b #f) 'false?) '✓]
    [((-b #t) 'true?) '✓]
    [((-b (? 𝔹?)) 'boolean?) '✓]
    [((-b 0) 'zero?) '✓]
    [((-b (? ℤ?)) 'integer?) '✓]
    [((-b (? ℝ?)) 'real?) '✓]
    [((-b (? ℂ?)) 'number?) '✓]
    [((-Struct t _) (-struct/pred t _)) '✓]
    [((? -Proc?) 'procedure?) '✓]
    [((? -Box?) 'box?) '✓]
    [((? -Vec?) 'vector?) '✓]
    ; Other cases
    [('● _) '?]
    [((? -base?) (? -o?)) 'X]
    [((-Struct t αs) (-Struct/C t γs))
     (for/fold ([r : Derivability '✓]) ([α αs] [γ γs])
       (match r
         ['X 'X]
         [r (match (σ⊢ σ α γ)
              ['✓ r]
              [r′ r′])]))]
    ; Safe default
    [(_ _) '?]))

;; Default (empty) external prover
(define Γ⊢ₓ : (Parameterof (-Γ -π′ → Derivability)) (make-parameter (const '?)))

(: Γ⊢ : -Γ -π′ → Derivability)
;; Check if given (pure) expression evaluates to non-#f
(define (Γ⊢ Γ π)
  (match (Γ⊢ᵢ Γ π)
    ['? ((Γ⊢ₓ) Γ π)]
    [A A]))

;; Internal simple proof system
(: Γ⊢ᵢ : -Γ -π′ → Derivability)
(define (Γ⊢ᵢ Γ π)
  ; Immediate cases
  (match π
    ; Base values other than #f are non-false
    [(-b #f) 'X]
    [(? -base?) '✓]
    [(-π@ (-struct/cons _ _) _) '✓]
    ; Simple tests on base values
    [(-π@ 'zero? (list (-b 0))) '✓]
    [(-π@ 'integer? (list (-b (? ℤ?)))) '✓]
    [(-π@ 'real? (list (-b (? ℝ?)))) '✓]
    [(-π@ 'number? (list (-b (? ℂ?)))) '✓]
    [(-π@ 'number? (list (-π@ (? -o/ℂℂ?) _))) '✓]
    [(-π@ 'false? (list (-b #f))) '✓]
    [(-π@ 'true? (list (-b #t))) '✓]
    [(-π@ 'boolean? (list (-b (? 𝔹?)))) '✓]
    [(-π@ 'string? (list (-b (? string?)))) '✓]
    [(-π@ 'symbol? (list (-b (? symbol?)))) '✓]
    [(-π@ 'procesure? (list (? -o?))) '✓]
    [(-π@ (-struct/pred t n) (list (-π@ (-struct/cons t n) _))) '✓]
    ; Simple equality tests. TODO: extend
    [(-π@ 'equal? (list π π)) '✓]
    [(-π@ 'equal? (list π₁ π₂))
     (if (set-member? Γ (-π@ 'equal? (list π₂ π₁))) '✓ '?)]
    [(-π@ '= (list π π)) '✓]
    ; Other unmentioned tests on concrete base values give #f
    [(-π@ (? -pred?) (list (? -base?))) 'X]
    ; Based on the head of the syntax and/or knowledge of primitive ops
    [(-π@ 'false? (list π)) (¬D (Γ⊢ Γ π))]
    [(-π@ 'number? (list (-π@ (? -o/ℂℂ?) _))) '✓]
    [(-π@ 'real? (list (-π@ (? -o/ℂℂ?) (list π))))
     (Γ⊢ᵢ Γ (-π@ 'real? (list π)))]
    [(-π@ 'integer? (list (-π@ (? -o/ℂℂ?) (list π))))
     (Γ⊢ᵢ Γ (-π@ 'integer? (list π)))]
    [(-π@ 'number? (list (-π@ (? -o/ℂℂℂ?) _))) '✓]
    [(-π@ 'real? (list (-π@ (? -o/ℂℂℂ?) (list π ...))))
     (for/fold ([d : Derivability '✓]) ([πᵢ π])
       (cond [(equal? d '✓) (match (Γ⊢ Γ (-π@ 'real? (list πᵢ)))
                              ['✓ '✓]
                              [_ '?])]
             [else '?]))]
    [(-π@ 'integer? (list (-π@ (or '+ '- '*) (list π ...))))
     (for/fold ([d : Derivability '✓]) ([πᵢ π])
       (cond [(equal? d '✓) (match (Γ⊢ Γ (-π@ 'integer? (list πᵢ)))
                              ['✓ '✓]
                              [_ '?])]
             [else '?]))]
    [(-π@ 'boolean? (list (-π@ (? -pred?) _))) '✓]
    [(-π@ (? -pred?) (list (-π@ (? -pred?) _))) 'X]
    [(-π@ (or 'integer? 'real? 'number?)
          (list (-π@ (or 'string-length 'procedure-arity 'vector-length) _))) '✓]
    [(-π@ 'number? (list (-π@ 'sqrt _))) '✓]
    [_
     ; Use assumtions to prove/refute
     (for/fold ([acc : Derivability '?]) ([πᵢ Γ])
       (match acc
         ['? (π⊢ πᵢ π)]
         [d d]))]))

(: π⊢ : -π -π′ → Derivability)
(define π⊢
  (match-lambda**
   [(_ 'ø) '?]
   [((-π@ 'false? (list π₁)) (-π@ 'false? (list π₂))) (π⊢ π₂ π₁)]
   [((-π@ 'false? (list π₁)) (? -π? π₂))
    (match (π⊢ π₂ π₁)
      ['✓ 'X]
      [_ '?])]
   [(π₁ (-π@ 'false? (list π₂))) (¬D (π⊢ π₁ π₂))]
   [((-π@ (? -pred? p) (list π)) (-π@ (? -pred? q) (list π)))
    (cond [(pred-implies? p q) '✓]
          [(pred-excludes? p q) 'X]
          [else '?])]
   [(_ _) '?]))

(: pred-excludes? : -pred -pred → 𝔹)
(define pred-excludes?
  (let ([groups (list (set 'zero? 'integer? 'real? 'number?)
                      (set 'true? 'boolean?)
                      (set 'false? 'boolean?))])
    (λ (p₁ p₂)
      (define s {set p₁ p₂})
      (not (for/or : 𝔹 ([g groups] #:when (subset? s g)) #t)))))

(: pred-implies? : -pred -pred → 𝔹)
(define pred-implies?
  (match-lambda**
   [(x x) #t]
   [('zero? (or 'integer? 'real? 'number?)) #t]
   [('integer? (or 'real? 'number?)) #t]
   [('real? 'number?) #t]
   [('true? 'boolean?) #t]
   [('false? 'boolean?) #t]
   [(_ _) #f]))

(: ¬D : Derivability → Derivability)
(define ¬D (match-lambda ['✓ 'X] ['X '✓] ['? '?]))

(: ∧D : Derivability Derivability → Derivability)
(define ∧D
  (match-lambda**
   [('✓ d) d]
   [(d '✓) d]
   [('X _) 'X]
   [(_ 'X) 'X]
   [(_ _) '?]))
