(module m racket/base
  (provide (contract-out [f (int? . -> . any)]))
  (define (f x)
    (if (= x 0) #f (cons x (g (- x 1)))))
  (define (g x)
    (if (= x 0) #t (cons x (f (- x 1))))))

(module main racket/base
  (require m)
  (define main (f •)))
