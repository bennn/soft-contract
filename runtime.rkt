#lang typed/racket/base
(provide (all-defined-out))
(require racket/match racket/set "abbrevs.rkt" "lib.rkt" "lang.rkt")

;;;;; Pure Subexpressions and Path Conditions
(define-type -Γ (Setof -π))
(define Γ∅ : -Γ ∅)
(define-data -π′
  'ø
  (subset: -π
    -base
    -x
    (struct -π@ [f : -π] [xs : (Listof -π)])))

(: Γ+ : -Γ -π′ → -Γ)
(define (Γ+ Γ π)
  (match π
    ['ø Γ]
    [(-π@ 'false? (list (-π@ 'false? (list π₁)))) (Γ+ Γ π₁)]
    [(? -π? π) (set-add Γ π)]))

(: π@ : -π′ (Listof -π′) → -π′)
(define (π@ π₁ πᵢ*)
  (match* (π₁ πᵢ*)
    [((? -π? π₁) (list (? -π? πᵢ) ...)) (-π@ π₁ (cast πᵢ (Listof -π)))]
    [(_ _) 'ø]))

(define (¬π [π : -π′])
  (cond [(-π? π) (-π@ 'false? (list π))]
        [else 'ø]))

(: restrict/π : -π′ (Setof Symbol) → -π′)
(define (restrict/π π xs)
  (match π
    ['ø 'ø]
    [(? -base? b) b]
    [(and π (-x z)) (cond [(set-member? xs z) π]
                          [else 'ø])]
    [(-π@ π πs) (π@ (restrict/π π xs) (for/list : (Listof -π′) ([π πs]) (restrict/π π xs)))]))

(: restrict/Γ : -Γ (Setof Symbol) → -Γ)
(define (restrict/Γ Γ xs)
  (for*/setof: -π ([π Γ] [π′ (in-value (restrict/π π xs))] #:when (-π? π′)) π′))

(: π/ : -π′ -π′ -π′ → -π′)
(define (π/ π x π′)
  (cond [(equal? π x) π′]
        [else (match π
                [(-π@ f zs) (π@ (π/ f x π′) (for/list ([z zs]) (π/ z x π′)))]
                [π π])]))

(: Γ/ : -Γ -π′ -π′ → -Γ)
(define (Γ/ Γ x πₓ)
  (for*/setof: -π ([π Γ] [π′ (in-value (π/ π x πₓ))] #:when (-π? π′)) π′))

(: FV/π : -π′ → (Setof Symbol))
(define FV/π
  (match-lambda
   ['ø ∅]
   [(-π@ f xs) (apply set-union (FV/π f) (map FV/π xs))]
   [(-x x) {set x}]
   [(? -base?) ∅]))

(: FV/Γ : -Γ → (Setof Symbol))
(define (FV/Γ Γ)
  (for/union-setof: Symbol ([π Γ]) (FV/π π)))

(: π+ : -π′ * → -π′)
(define (π+ . π′)
  (or (for/or : (Option -π) ([π π′] #:when (-π? π)) π)
      'ø))

(: W+ : -W -π′ * → -W)
(define (W+ W . π′)
  (match-define (-W α π) W)
  (cond [(-π? π) W]
        [else (-W α (apply π+ π′))]))

;;;;; Enviroment + Closure
(define-type -ρ (Map Symbol -W))
(define ρ∅ : -ρ (hash))

(: ρ+ : (case->
         [-ρ Symbol -W → -ρ]
         [-ρ (Listof Symbol) (Listof -W) → -ρ]
         [-ρ (Listof (Pairof Symbol -W)) → -ρ]))
(define ρ+
  (case-lambda
    [(ρ x* W*)
     (cond [(list? x*) (for/fold ([ρ ρ]) ([x x*] [W W*])
                         (hash-set ρ x W))]
           [else (hash-set ρ x* W*)])]
    [(ρ l) (for/fold ([ρ ρ]) ([xW l]) (hash-set ρ (car xW) (cdr xW)))]))

(: close : -v -ρ -Γ → (Values -V -π′))
(define (close v ρ Γ)
  (match v
    [(-λ xs e) (values (-Λ xs e ρ Γ) 'ø)]
    [(-rec/c e) (values (-Rec/C e ρ Γ) 'ø)]
    ['● (values '● 'ø)]
    [(? -base? b) (values b b)]))

;;;;; Point
(define-data -E
  (subset: -A
    (struct -blm [l⁺ : L] [lº : L] [tag : Any #|TODO|#])
    (struct -W [α : -α] [π : -π′]))
  (struct -↓ [e : -e] [ρ : -ρ])
  (struct -Mon [l³ : L³] [c : -α] [v : -α])
  (struct -FC [l : L] [c : -α] [v : -W]))

(define-type -a (U -α -blm))

(define-data -V
  '●
  -b
  (struct -Struct [tag : Struct-Tag] [fields : (Listof -α)])
  (subset: -Box
    (struct -Box/Raw [unboxed : -α])
    (struct -Box/Wrapped [l³ : L³] [c : -α] [b : -α]))
  (subset: -Vec
    (struct -Vec/Raw [vals : (Listof -α)])
    (struct -Vec/Wrapped [l³ : L³] [c : -α] [v : -α]))
  #;(subset: -Ht
    (struct -Ht/Raw [m : (Map -α -α)])
    (struct -Ht/Wrapped [l³ : L³] [c : -α] [h : -α]))
  (subset: -Proc
    -o
    (struct -Λ [xs : (Listof Symbol)] [e : -e] [ρ : -ρ] [Γ : -Γ])
    (struct -Arr [l³ : L³] [c : -α] [f : -α]))
  ; Contract
  (struct -Λ/C [dom : (Listof (Pairof Symbol -α))] [rng : -e] [ρ : -ρ] [Γ : -Γ])
  (struct -Struct/C [tag : Struct-Tag] [fields : (Listof -α)])
  (struct -Rec/C [e : -e] [ρ : -ρ] [Γ : -Γ])
  (struct -Box/C [c : -α])
  (struct -Vec/C [cs : (Listof -α)]))

(: unzip/W : (Listof -W) → (Values (Listof -α) (Listof -π′)))
(define (unzip/W Ws)
  (for/lists ([αs : (Listof -α)] [πs : (Listof -π′)]) ([W Ws])
    (values (-W-α W) (-W-π W))))

;;;;;; Store
(define-type/pred -α (U -base ℤ)) ; TODO
(define-type -σ (Map* -α -V))

(: σ@ : -σ (U -α -W) → (Setof -V))
(define (σ@ σ x)
  (cond [(-W? x) (hash-ref σ (-W-α x))]
        [else (hash-ref σ x)]))

(: σ+′ : -V -π′ -Γ → Any)
(define (σ+′ V π Γ)
  (match V
    ['● '●]
    [(? -base? b) b]
    [(-Λ/C dom rng _ Γ) (list 'λ/c (map (inst car Symbol Any) dom) rng Γ)]
    [(-Struct t _) (list 'struct t π Γ)]
    [(-Struct/C t _) (list 'struct/c t π Γ)]
    [(-Rec/C e ρ Γ) (list 'rec/c e)]
    [(-Box/Raw _) (list 'box Γ)]
    [(-Box/Wrapped l³ _ _) (list 'box/wrapped l³ Γ)]
    [(-Vec/Raw l) (list 'vec (length l) Γ)]
    [(-Vec/Wrapped l³ _ _) (list 'vec/wrapped l³ Γ)]
    [(-Λ xs e _ Γ) (list 'λ xs e)]
    [(-Arr l³ _ _) (list 'Arr l³ Γ)]))

(: addr→ℤ : Any → -α)
(define addr→ℤ
  (let ([m : (Map Any ℤ) (make-hash)])
    (λ (addr)
      (cond
       [(-base? addr) addr]
       [else (hash-ref! m addr (λ () (hash-count m)))]))))

(: σ+ : ([-V] [-π′ -Γ] . ->* . -α))
;; I switch back and forth for debugging. Integers are nicer to read sometimes.
(define (σ+ V [π 'ø] [Γ Γ∅])
  (addr→ℤ (σ+′ V π Γ)))
#;(define σ+ σ+′)

(define σ∅ : -σ (hash))
(define σ₀ (for/fold : -σ ([σ σ∅]) ([V (list 'number? 'real? 'integer? 'false? 'zero? 'vector? 'box?
                                             -tt -ff (+b 0) (+b 1) (-Struct 'void '()))])
             (let ([α (σ+ V 'ø Γ∅)])
               (⊔ σ α V))))


;;;;; Kontinuation
;; Kontinuation frame
(define-data -Φ
  (struct -ifₖ [t : -E] [e : -E])
  (struct -@ₖ [ctx : L] [e* : (Listof -E)] [v* : (Listof -W)])
  (struct -letₖ [current : Symbol]
                [e* : (Listof (Pairof Symbol -e))] [v* : (Listof (Pairof Symbol -W))]
                [e : -e] [ρ : -ρ]) 
  ; Return stuff
  (struct -rtₖ [ctx : -Γ] [bindings : (Listof (Pairof -x -π))])
  (struct -rt/letₖ [ctx : (Setof Symbol)])
  ; Contract stuff
  (struct -λ/cₖ [current : Symbol]
                [c : (Listof (Pairof Symbol -e))] [c↓ : (Listof (Pairof Symbol -α))]
                [d : -e] [ρ : -ρ])
  (struct -struct/cₖ [t : Struct-Tag] [c : (Listof -e)] [ρ : -ρ] [c↓ : (Listof -W)])
  ; Box stuff
  'box
  (struct -set-box!ₖ₁ [ctx : L] [e : -e] [ρ : -ρ])
  (struct -set-box!ₖ₂ [ctx : L] [α : -α])
  (struct -unboxₖ [ctx : L])
  ; Vector stuff
  (struct -vectorₖ [e* : (Listof -e)] [ρ : -ρ] [v* : (Listof -α)])
  (struct -vector-refₖ₁ [ctx : L] [i : -e] [ρ : -ρ])
  (struct -vector-refₖ₂ [ctx : L] [v : -W])
  (struct -vector-set!ₖ₁ [ctx : L] [i : -e] [val : -e] [ρ : -ρ])
  (struct -vector-set!ₖ₂ [ctx : L] [vec : -W] [val : -e] [ρ : -ρ])
  (struct -vector-set!ₖ₃ [ctx : L] [vec : -W] [i : -W])
  ; Monitoring
  (struct -monᶜₖ [l³ : L³] [e : -E])
  (struct -monᵛₖ [l³ : L³] [c : -W])
  (struct -indyₖ [l³ : L³]
                 [dom↓ : (Listof -W)]
                 [args : (Listof -W)] [args/checked : (Listof -W)]
                 [rng : (Option -↓)])
  )
(define-type -κ (Listof -Φ))

;;;;; Program state
(define-data -Σ
  (subset: -Σ/final
    (struct -Σ/ok [evaled-p : -p↓] [σ : -σ])
    (struct -Σ/err [evaled-p : -p↓] [σ : -σ] [current : Binding] [blm : -blm]))
  (struct -Σ/kont [evaled-p : -p↓] [pending-p : (Listof -m)]
                  [evaled-m : -m↓] [pending-m : (Listof Cmd)] [current : Binding]
                  [ς : -ς/kont]))

(define-data -ς
  (struct -Ans [σ : -σ] [Γ : -Γ] [a : -A])
  (struct -ς/kont [e : -E] [Γ : -Γ] [σ : -σ] [κ : -κ]))

(define-type -p↓ (Map L -m↓))
(define p↓∅ : -p↓ (hash))
(struct -m↓ ([name : Symbol]
             [provides : (Map Symbol -α)]
             [requires : (Setof Symbol)]
             [defines : (Map Symbol -α)])
        #:transparent)
(struct Binding ([action : (U 'def 'dec)] [name : Symbol]) #:transparent)
(struct Cmd ([binding : Binding] [e : -e]) #:transparent)

;; Convert module to list of bindings to compute
(: m→cmds : -m → (Values -m↓ (NeListof Cmd)))
(define m→cmds
  (let ([m∅ : (Map Symbol -α) (hash)])
    (match-lambda
     [(-m l provs reqs defs)
      (values
       (-m↓ l m∅ (for/setof: Symbol ([req reqs]) (car req)) m∅)
       (cast
        (append (for/list : (Listof Cmd) ([p provs]) (Cmd (Binding 'dec (car p)) (cdr p)))
                (for/list : (Listof Cmd) ([p defs]) (Cmd (Binding 'def (car p)) (cdr p))))
        (NeListof Cmd)))])))

;; Add new binding to (partially evaluated) module
(: m+ : -m↓ Binding -α → -m↓)
(define (m+ m↓ b α)
  (match-define (-m↓ l provs reqs defs) m↓)
  (match b
    [(Binding 'dec x) (-m↓ l (hash-set provs x α) reqs defs)]
    [(Binding 'def x) (-m↓ l provs reqs (hash-set defs x α))]))

(: gc : -Σ → -Σ)
(define (gc Σ) Σ) ; TODO
