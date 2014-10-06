#lang typed/racket/base
(provide (all-defined-out))
(require racket/match racket/set racket/function racket/list
         "abbrevs.rkt" "lib.rkt" "lang.rkt")

(: show/p : -p → (Listof Any))
(define (show/p p) (map show/m p))

(: show/m : -m → (Listof Any))
(define (show/m m)
  (: show/provs : (Listof (Pairof Symbol -e)) → (Listof Any))
  (define (show/provs provs)
    `(provide
      (contract-out
       ,@(for/list : (Listof Any) ([prov provs]) `[,(car prov) ,(show/e (cdr prov))]))))
  
  (: show/reqs : (Listof (Pairof L (Setof Symbol))) → (Listof Any))
  (define (show/reqs reqs)
    `(require ,@(for/list : (Listof Any) ([req reqs]) `(only-in ,(car req) ,@(set->list (cdr req))))))
  
  (: show/defs : (Listof (Pairof Symbol -e)) → (Listof Any))
  (define (show/defs defs)
    (for/list : (Listof Any) ([d defs])
      (match (cdr d)
        [(-λ xs e) `(define (,(car d) ,@xs) ,(show/e e))]
        [e `(define ,(car d) ,(show/e e))])))
  
  (match-let ([(-m l provs reqs defs) m])
    `(module ,l
      ,@(cond [(empty? provs) '()]
              [else (list (show/provs provs))])
      ,@(cond [(empty? reqs) '()]
              [else (list (show/reqs reqs))])
      ,@(show/defs defs))))

(: show/e : (U -e (Listof -e)) → Any)
(define (show/e e)
  (match e
    ;; Special cases
    [(-λ (list _) (-b #t)) 'any]
    [(-λ (list _) (-b #f)) 'none]
    [(-λ (list x) (-x x)) 'id]
    [(-if i t (-b #f)) `(and ,(show/e i) ,(show/e t))]
    [(-let (list (cons x e₁)) (-if (-x x) (-x x) e₂))
     `(or ,(show/e e₁) ,(show/e e₂))]
    ;; General cases
    [(? list? l) (map show/e l)]
    [(-λ xs e) `(λ ,xs ,(show/e e))]
    [(-b b) b]
    [(? -o? o) (show/o o)]
    [(-x x) x]
    [(-ref _ x _) x]
    [(-@ _ f xs) `(,(show/e f) ,@(show/e xs))]
    [(-@/havoc x) `(havoc ,x)]
    [(-if i t e) `(if ,(show/e i) ,(show/e t) ,(show/e e))]
    [(-let bindings e) `(let ,(for/list : (Listof Any) ([b bindings])
                                (list (car b) (show/e (cdr b))))
                             ,(show/e e))]
    [(-amb es) `(amb ,@(for/list : (Listof Any) ([e es]) (show/e e)))]
    [(-rec/c e) `(rec/c ,(show/e e))]
    [(-λ/c dom rng)
     (let-values ([(xs cs) (unzip dom)]
                  [(fv) (FV rng)])
       (cond
        [(for/or : 𝔹 ([x xs] #:when (set-member? fv x)) #t)
         `(,@(for/list : (Listof Any) ([x xs] [c cs]) `[,x ,(show/e c)]) ↦ ,(show/e rng))]
        [else `(,@(for/list : (Listof Any) ([c cs]) (show/e c)) ↦ ,(show/e rng))]))]
    [(-struct/c t cs)
     `(,(string->symbol (format "~a/c" t)) ,@(for/list : (Listof Any) ([c cs]) (show/e c)))]))

(: show/o : -o -> Symbol)
(define show/o
  (match-lambda
   [(? symbol? s) s]
   [(-struct/pred t _) (string->symbol (format "~a?" t))]
   [(-struct/acc 'cons 2 0) 'car]
   [(-struct/acc 'cons 2 1) 'cdr]
   [(-struct/acc t _ i) (string->symbol (format "~a@~a" t i))]
   [(-struct/cons t _) t]))
