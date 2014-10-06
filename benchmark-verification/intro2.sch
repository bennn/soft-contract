(module f racket/base
  (provide (contract-out [f (int? (int? . -> . any) . -> . any)]))
  (define (f x g) (g (+ x 1))))
(module h racket/base
  (provide (contract-out [h ((and/c int? (>/c 0)) . -> . any)]))
  (define (h y) 'unit))
(module main racket/base
  (provide (contract-out [main (int? . -> . any)]))
  (require f h)
  (define (main n)
    (if (>= n 0) (f n h) 'unit)))

(module main racket/base
  (require main)
  (define top (main •)))
