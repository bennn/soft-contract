(module lastpair racket/base
  (provide (contract-out [lastpair (cons? . -> . cons?)]))
  (define (lastpair x)
    (if (cons? (cdr x)) (lastpair (cdr x)) x)))

(module main racket/base
  (require lastpair)
  (define top (lastpair (cons • •))))
