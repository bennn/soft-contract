(module f racket/base
  (provide
   (contract-out 
    [f ((or/c str? num?) . -> . num?)]))
  (define (f x)
    (if (num? x) (add1 x) (str-len x))))

(module g racket/base
  (provide (contract-out [g (any . -> . num?)]))
  (require f)
  (define (g x)
    (if (or (num? x) (str? x)) (f x) 0)))

(module main racket/base
  (require g)
  (define top (g •)))
