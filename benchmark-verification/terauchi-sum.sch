(module assert racket/base
  (provide (contract-out [assert ((not/c false?) . -> . any)])))

(module m racket/base
  (provide (contract-out [main (-> any)]))
  (require assert)
  (define (sum x) (if (<= x 0) 0 (+ x (sum (- x 1)))))
  (define (main) (assert (<= 100 (sum 100)))))

(module main racket/base
  (require m)
  (main))
