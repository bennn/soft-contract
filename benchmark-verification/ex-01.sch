(module f racket/base
  (provide (contract-out [f (any . -> . num?)]))
  (define (f x)
    (if (num? x) (add1 x) 0)))

(module main racket/base
  (require f)
  (define top (f •)))
