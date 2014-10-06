(module f racket/base
  (provide (contract-out [f ((or/c str? num?) . -> . num?)]))
  (define (f x)
    (if (num? x) (add1 x) (str-len x))))

(module main racket/base
  (require f)
  (define top (f •)))
