(module f racket/base
  (provide (contract-out [f ((or/c num? str?) str? . -> . num?)]))
  (define (f x y)
    (if (and (num? x) (str? y))
        (+ x (str-len y))
        (str-len x))))
(module main racket/base
  (require f)
  (define top (f • •)))
