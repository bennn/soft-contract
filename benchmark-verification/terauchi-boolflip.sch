(module assert racket/base
  (provide (contract-out [assert ((not/c false?) . -> . any)])))
(module m racket/base
  (provide (contract-out [main (-> any)]))
  (require assert)
  (define (f x y) (if x (f y x) (g x y)))
  (define (g x y) (assert y))
  (define (h x) (assert x))
  (define (main) (if (< 0 1) (f (< 0 1) (< 1 0)) (h (< 1 0)))))
(module main racket/base
  (require m)
  (define top (main)))
