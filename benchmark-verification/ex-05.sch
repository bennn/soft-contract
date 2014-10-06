(module f racket/base
  (provide (contract-out [f (any any . -> . num?)]))
  (define (f x y)
    (if (and (num? x) (str? y)) (+ x (str-len y)) 0)))
(module main racket/base
  (require f)
  (define top (f • •)))
