#lang typed/racket/base
(provide (all-defined-out))
(require racket/match racket/set "abbrevs.rkt" "lib.rkt")

;; Module name
(define-type L Symbol)
(define-type L³ (List Symbol Symbol Symbol))

;; Current base types
(define-type Base (U ℂ 𝔹 String Symbol))

(define-type Struct-Tag Symbol #|TODO|#)

;; I prefix types with `-` just so I can use 1-char variable without shadowing types.

;; A program is a sequence of module definitions
(define-type -p (NeListof -m))

;; A module:
;  - exports names wrapped in contracts
;  - requires names from other modules
;  - defines structs and values
(struct -m ([name : L]
            [provides : (Listof (Pairof Symbol -e))]
            [requires : (Listof (Pairof L (Setof Symbol)))]
            [defines : (NeListof (Pairof Symbol -e))])
        #:transparent)

;; Annotated terms
(define-data -e
  (subset: -v
    (struct -λ [xs : (Listof Symbol)] [body : -e])
    '●
    (subset: -base
      (struct -b [unboxed : Base])
      (subset: -o
        (subset: -o¹
          (subset: -pred
            (struct -struct/pred [tag : Struct-Tag] [arity : ℕ])
            'number? 'real? 'integer? 'false? 'boolean? 'string? 'symbol? 'procedure? 'box? 'vector?
            'zero?)
          (struct -struct/acc [tag : Struct-Tag] [arity : ℕ] [index : ℕ])
          (subset: -o/ℂℂ 'add1 'sub1)
          'string-length 'vector-length 'sqrt
          'procedure-arity)
        (subset: -o²
                 'equal? '=
          (subset: -o/ℂℂℂ '+ '- '* '/)
          (subset: -o/ℝℝ𝔹 '> '< '≥ '≤))
        (subset: -o³ 'vector-set!)
        (struct -struct/cons [tag : Struct-Tag] [arity : ℕ])))
    (struct -rec/c [x : -e]))
  (struct -x [name : Symbol])
  (struct -ref [ctx : L] [name : Symbol] [in : L])
  (struct -@ [ctx : L] [f : -e] [xs : (Listof -e)])
  (struct -@/havoc [x : -x]) ; Hack for havoc
  (struct -if [i : -e] [t : -e] [e : -e])
  (struct -let [bindings : (Listof (Pairof Symbol -e))] [body : -e])
  (struct -amb [es : (Setof -e)])
  ; Contract stuff
  #;(-μ/c [x : Symbol] [c : -e])
  (struct -λ/c [dom : (Listof (Pairof Symbol -e))] [rng : -e])
  (struct -struct/c [tag : Struct-Tag] [fields : (Listof -e)])
  #;(-and/c [l : -e] [r : -e])
  #;(-or/c [l : -e] [r : -e])
  #;(-not/c [c : -e])
  ; Box stuff
  (struct -box [unboxed : -e])
  (struct -unbox [ctx : L] [box : -e])
  (struct -set-box! [ctx : L] [box : -e] [val : -e])
  ; Vector stuff
  (struct -vector [fields : (Listof -e)])
  (struct -vector-ref [ctx : L] [vec : -e] [index : -e])
  (struct -vector-set! [ctx : L] [vec : -e] [index : -e] [val : -e]))

;; Interned constructors
(define/memoeq (+b [b : Base]) : -b (-b b))
(define/memoeq (+x [x : (U ℕ Symbol)]) : -x
  (cond ; ℕ case is for internal use, assume no overlap with user's
   [(integer? x) (-x (genₓ x))]
   [else (-x x)]))

;; Generate internal symbol for nth index
(define/memoeq (genₓ [n : ℕ]) : Symbol
  (string->symbol (format "⋆~a" (subscript n))))

(: FV : (Rec X (U -e (Listof X))) → (Setof Symbol))
(define FV
  (match-lambda
   [(? list? l) (for/union-setof: Symbol ([e l]) (FV e))]
   [(-λ xs e) (set-remove/l (FV e) xs)]
   [(-rec/c e) (FV e)]
   [(-x x) {set x}]
   [(-@ _ f xs) (set-union (FV f) (FV xs))]
   [(-if e e₁ e₂) (set-union (FV e) (FV e₁) (FV e₂))]
   [(-let xes e) (let-values ([(xs es) (unzip xes)])
                   (set-union (FV es) (set-remove/l (FV e) xs)))]
   [(-amb es) (for/union-setof: Symbol ([e es]) (FV e))]
   [(-λ/c cxs d) (let-values ([(xs cs) (unzip cxs)])
                   (set-union (FV cs) (set-remove/l (FV d) xs)))]
   [(-struct/c _ cs) (FV cs)]
   #;[(-and/c l r) (set-union (FV l) (FV r))]
   #;[(-or/c l r) (set-union (FV l) (FV r))]
   #;[(-not/c c) (FV c)]
   [(-box e) (FV e)]
   [(-unbox _ e) (FV e)]
   [(-set-box! _ b v) (set-union (FV b) (FV v))]
   [(-vector (list e ...)) (FV e)]
   [(-vector-ref _ v i) (set-union (FV v) (FV i))]
   [(-vector-set! _ v i e) (set-union (FV v) (FV i) (FV e))]
   [(? -base?) ∅]))

;; Reused terms
(define/*
  [-tt (+b #t)]
  [-ff (+b #f)]
  [-any/c (-λ `(x) -tt)]
  [-none/c (-λ `(x) -ff)]
  [-empty (-@ 'Λ (-struct/cons 'empty 0) (list))] ; hack
  [-empty/c (-struct/pred 'empty 0)]
  [-cons (-struct/cons 'cons 2)]
  [-car (-struct/acc 'cons 2 0)]
  [-cdr (-struct/acc 'cons 2 1)]
  [-cons? (-struct/pred 'cons 2)]
  [-zero (+b 0)]
  [-one (+b 1)]
  [-pred/c (-λ/c (list (cons '•₀ -any/c)) 'boolean?)])

(define (-cons/c [c : -e] [d : -e])
  (-struct/c 'cons (list c d)))

(: ¬l : L³ → L³)
(define ¬l (match-lambda [(list l₊ l₋ lₒ) (list l₋ l₊ lₒ)]))

(: amb : (Listof -e) → -e)
(define (amb es)
  (let ([s (for/fold ([acc : (Setof -e) ∅]) ([e es])
             (match e ; try to avoid nested amb
               [(-amb s) (set-union acc s)]
               [e (set-add acc e)]))])
    (match (set-count s)
      [1 (set-first s)]
      [_ (-amb s)])))

(define (-and/c [l : L] [cs : (Listof -e)])
  (let go : -e ([cs : (Listof -e) cs])
    (match cs
      ['() -any/c]
      [(list c) c]
      [(cons c₁ cᵣ) (-@ l (-struct/cons 'and/c 2) (list c₁ (go cᵣ)))])))

(define (-or/c [l : L] [cs : (Listof -e)])
  (let go : -e ([cs : (Listof -e) cs])
    (match cs
      ['() -none/c]
      [(list c) c]
      [(cons c₁ cᵣ) (-@ l (-struct/cons 'or/c 2) (list c₁ (go cᵣ)))])))

(define (-not/c [l : L] [c : -e])
  (-@ l (-struct/cons 'not/c 1) (list c)))
