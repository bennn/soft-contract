(module f racket/base
  (provide (contract-out [f (cons? . -> . symbol?)]))
  (require g)
  (define (f p)
    (if (and (num? (car p)) (num? (cdr p))) (g p) 'no)))

(module g racket/base
  (provide (contract-out [g ((cons/c num? num?) . -> . symbol?)])))

(module main racket/base
  (require f)
  (define top (f •)))
