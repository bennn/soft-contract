(module assert racket/base
  (provide (contract-out [assert ((not/c false?) . -> . any)])))

(module m racket/base
  (provide (contract-out [main (-> any)]))
  (require assert)
  (define (sum x y k)
    (if (<= x 0) (k y) (sum (- x 1) (+ x y) k)))
  (define (check x) (assert (<= 100 x)))
  (define (main) (sum 100 0 check)))

(module main racket/base
  (require m)
  (define top (main)))
