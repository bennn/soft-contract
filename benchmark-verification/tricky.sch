(module f racket/base
  (provide (contract-out [f (int? . -> . int?)]))
  (define (f x)
    (if (zero? x) 0
        (if (zero? (f (sub1 x))) 7 8))))

(module main racket/base
  (require f)
  (define top (f •)))
