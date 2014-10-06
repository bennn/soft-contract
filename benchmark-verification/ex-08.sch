(module strnum? racket/base
  (provide
   (contract-out
    [strnum? ([x : any] . -> . (and/c bool? (λ (a) (equal? a (or (str? x) (num? x))))))]))
  (define (strnum? x)
    (or (str? x) (num? x))))
(module main racket/base
  (require strnum?)
  (define top (strnum? •)))
