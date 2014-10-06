(module f racket/base
  (provide
   (contract-out
    [f (cons? . -> . num?)]))
  (define (f p)
    (if (num? (car p)) (add1 (car p)) 7)))

(module main racket/base
  (require f)
  (define top (f •)))
