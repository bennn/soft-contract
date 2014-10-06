#lang typed/racket/base
(provide
 ;; Syntax
 define-type/pred
 define/memoeq define/memo define-data define-set define/*
 match? match-λ? match/nd match*/nd let/nd within-time: :* @?
 for/union-setof: for/setof: for*/setof:
 errorf
 ;; Values
 set-add/l set-remove/l unzip hash-set-new hash-invert ⊔ ⊔! subscript
 list-set
 (rename-out [set-union ∪]))
(require racket/set racket/match
         (for-syntax racket/base racket/match racket/syntax)
         "abbrevs.rkt")

;; Define the memoized version for a function with *one* *unboxed* argument
(define-syntax define/memoeq
  (syntax-rules (:)
    [(_ (f [x : τ]) : σ e ...)
     (define f : (τ → σ)
       (let ([m : (HashTable τ σ) (make-hasheq)])
         (λ ([x : τ]) (hash-ref! m x (λ () e ...)))))]))
(define-syntax define/memo
  (syntax-rules (:)
    [(_ (f [x : τ] ...) : σ e ...)
     (define f : (τ ... → σ)
       (let ([m : (HashTable (List τ ...) σ) (make-hash)])
         (λ ([x : τ] ...) (hash-ref! m (list x ...) (λ () e ...)))))]))

;; Define data hierarchy
(define-syntax-rule (define-data τ σ ...)
  (define-data′ τ (σ ...) ()))
(define-syntax (define-data′ stx)
  (syntax-case stx (subset: struct)
    [(_ τ () (σ ...)) #'(define-type/pred τ (U σ ...))]
    [(_ τ ((subset: τ′ clauses′ ...) clauses ...) (σ ...))
     #'(begin (define-data′ τ′ (clauses′ ...) ())
              (define-data′ τ (clauses ...) (τ′ σ ...)))]
    [(_ τ ((struct k f ...) clauses ...) (σ ...))
     #'(begin (struct k (f ...) #:transparent)
              (define-data′ τ (clauses ...) (k σ ...)))]
    [(_ τ (τ₁ clauses ...) (σ ...))
     #'(define-data′ τ (clauses ...) (τ₁ σ ...))]))

;; Define set with shortened syntax for (imperative) adding and membership testing
(define-syntax (define-set stx)
  (syntax-case stx (:)
    [(_ s : τ)
     (with-syntax ([s-has? (format-id #'s "~a-has?" #'s)]
                   [s-add! (format-id #'s "~a-add!" #'s)])
       #'(begin (define s : (Setof τ) ∅)
                (define (s-has? [x : τ]) : 𝔹 (set-member? s x))
                (define (s-add! [x : (U τ (Setof τ))])
                  (set! s (cond [(set? x) (set-union s x)]
                                [else (set-add s x)])))))]))

(define-syntax-rule (match? v p ...) (match v [p #t] ... [_ #f]))
(define-syntax-rule (match-λ? p ...) (match-lambda [p #t] ... [_ #f]))

;; Application with implicit #f short-cutting
(define-syntax @?
  (syntax-rules (!)
    [(@? f e ...) (@?′ f (e ...) ())]))
(define-syntax @?′
  (syntax-rules (!)
    [(@?′ f () (x ...)) (f x ...)]
    [(@?′ f ((! e₁) e ...) (x ...))
     ; Can't delay e₁ to later, the order could surprising for macro user
     (let ([x₁ e₁]) (@?′ f (e ...) (x ... x₁)))]
    [(@?′ f (e₁ e ...) (x ...))
     ; TODO: will snoc-ing an element be that bad?
     (let ([x₁ e₁])
       (if x₁ (@?′ f (e ...) (x ... x₁)) #f))]))

;; Comprehension form for set
(define-syntax-rule (for/setof: τ (c ...) e ...)
  (for/fold ([acc : (Setof τ) ∅]) (c ...)
    (set-add acc (begin e ...))))
(define-syntax-rule (for*/setof: τ (c ...) e ...)
  (for*/fold ([acc : (Setof τ) ∅]) (c ...)
    (set-add acc (begin e ...))))


;;;;; Non-determinism operations

;; Non-deterministic match. The types are to make pattern matching less awkward
(define-syntax match/nd
  (syntax-rules (: →)
    [(_ : (α → β) v [p e ...] ...)
     (let ([x : (ND α) v]
           [f : (α → (ND β))
              (match-lambda [p e ...] ... [x (error "match/nd unmatched: " x)])])
       (cond
        [(set? x) (for/fold : (Setof β) ([acc : (Setof β) ∅]) ([xᵢ x])
                    (let ([y (f (cast xᵢ α))])
                      (cond [(set? y) (set-union acc (cast y (Setof β)))]
                            [else (set-add acc y)])))]
        [else (f x)]))]))

(define-syntax match*/nd
  (syntax-rules (: →)
    [(_ : τ e* c ...) (match*/nd′ τ e* (c ...) ())]))
(define-syntax match*/nd′
  (syntax-rules (: →)
    [(_ (α₁ α ... → β) (e₁ e ...) c* (x ...))
     (let/nd : (α₁ → β) ([x₁ e₁])
       (match*/nd′ (α ... → β) (e ...) c* (x ... x₁)))]
    [(_ (→ β) () (c ...) (x ...))
     (match* (x ...) c ...)]))

(define-syntax let/nd
  (syntax-rules (: →)
    [(let/nd : (α → β) ([x v]) e ...)
     (let ([x v]
           [f : (α → (ND β)) (λ (x) e ...)])
       (cond
        [(set? x) (for/fold ([acc : (Setof β) ∅]) ([xᵢ x])
                    (let ([y (f (cast xᵢ α))])
                      (cond [(set? y) (set-union acc (cast y (Setof β)))]
                            [else (set-add acc y)])))]
        [else (f x)]))]))

;; Define the same type for multiple identifers
(define-syntax (:* stx)
  (match (syntax->datum stx)
    [`(,_ ,x ... : ,τ ...)
     (datum->syntax stx `(begin ,@(for/list ([xᵢ x]) `(: ,xᵢ : ,@τ))))]))

;; Define multiple values
(define-syntax-rule (define/* (x e ...) ...)
  (begin (define x e ...) ...))


;; Evaluate an expression within given #seconds
;; Return singleton list of value, or #f if timeout
(define-syntax-rule (within-time: τ n e ...)
  (let ([c : (Channelof (Option (List τ))) (make-channel)])
    (let ([t₁ (thread (λ () (channel-put c (list (begin e ...)))))]
          [t₂ (thread (λ () (sleep n) (channel-put c #f)))])
      (match (channel-get c)
        [#f (kill-thread t₁) #f]
        [ans (kill-thread t₂) ans]))))


;;;;; Set operations

(:* set-remove/l set-add/l : (∀ (X) (Setof X) (Listof X) → (Setof X)))
;; Add/remove elements in list from set
(define (set-remove/l s l)
  (for/fold ([acc s]) ([x l]) (set-remove acc x)))
(define (set-add/l s l)
  (for/fold ([acc s]) ([x l]) (set-add acc x)))

(define-syntax-rule (for/union-setof: τ (c ...) e ...)
  (for/fold : (Setof τ) ([acc : (Setof τ) ∅]) (c ...)
    (set-union acc (begin e ...))))


;;;;; List operations

(: unzip : (∀ (X Y) (Listof (Pairof X Y)) → (Values (Listof X) (Listof Y))))
(define (unzip xs)
  (for/lists ([ls : (Listof X)] [rs : (Listof Y)]) ([x xs])
    (values (car x) (cdr x))))

(: list-set : (∀ (X) (Listof X) ℕ X → (Listof X)))
(define (list-set l i v)
  (cond [(null? l) (error "list-set: empty list")]
        [(zero? i) (cons v (cdr l))]
        [else (cons (car l) (list-set (cdr l) (- i 1) v))]))

;;;;; Map operations

(: hash-set-new : (∀ (X Y) (Map X Y) X Y (Y → String) → (Map X Y)))
;; Add mapping only if it has not existed before, otherwise raise error
(define (hash-set-new m x v s)
  (cond [(hash-has-key? m x) (error (s (hash-ref m x)))]
        [else (hash-set m x v)]))

(: hash-invert : (∀ (X Y) (Map X Y) → (Map Y (Setof X))))
;; Invert the given mapping
(define (hash-invert m)
  (for/fold ([m⁻¹ : (Map Y (Setof X)) (hash)]) ([(x y) m])
    (hash-update m⁻¹ y (λ ([xs : (Setof X)]) (set-add xs x)) (λ () ∅))))


;;;;; Multimap operations
(: ⊔ : (∀ (X Y) (Map* X Y) X Y → (Map* X Y)))
(define (⊔ m x y)
  (hash-update m x (λ ([ys : (Setof Y)]) (set-add ys y)) (λ () ∅)))

(: ⊔! : (∀ (X Y) (Map* X Y) X Y → Void))
(define (⊔! m x y)
  (hash-update! m x (λ ([ys : (Setof Y)]) (set-add ys y)) (λ () ∅)))

;;;;; Syntax for creating and running state machines.
;;;;; Pretty printing
(: subscript : ℕ → String)
(define (subscript n)
  (cond [(< n 10) (vector-ref #("₀" "₁" "₂" "₃" "₄" "₅" "₆" "₇" "₈" "₉") n)]
        [else (let-values ([(q r) (quotient/remainder n 10)])
                (string-append (subscript q) (subscript r)))]))

;; error with format
(define-syntax-rule (errorf s x ...)
  (error (format s x ...)))
