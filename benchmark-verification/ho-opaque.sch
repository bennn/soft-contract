(module db1 racket/base
  (provide (contract-out [db1 ([zero? . -> . zero?] . -> . [zero? . -> . zero?])]))
  (define (db1 f)
    (λ (x) (f (f x)))))

(module f racket/base (provide (contract-out [f (zero? . -> . num?)])))

(module main racket/base
  (require db1 f)
  (define top ((db1 f) 0)))
