(module assert racket/base
  (provide (contract-out [assert ((not/c false?) . -> . any)])))

(module m racket/base
  (provide (contract-out [main (-> any)]))
  (require assert)
  (define (sum x) (if (<= x 0) 0 (+ x (sum (- x 1)))))
  (define (h y) (assert (<= y (sum y))))
  (define (main) (h 0)))

(module main racket/base
  (require m)
  (define top (main)))
