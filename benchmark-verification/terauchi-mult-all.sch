(module assert racket/base
  (provide (contract-out [assert ((not/c false?) . -> . any)])))

(module m racket/base
  (provide (contract-out [main (-> any)]))
  (require assert)
  (define (mult x y)
    (if (or (<= x 0) (<= y 0)) 0 (+ x (mult x (- y 1)))))
  (define (h y) (assert (<= y (mult y y))))
  (define (main) (h 0)))

(module main racket/base
  (require m)
  (define top (main)))
