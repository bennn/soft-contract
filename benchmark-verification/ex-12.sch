(module carnum? racket/base
  (provide
   (contract-out
    [carnum? ([p : cons?] . -> . (and/c bool? (λ (a) (equal? a (num? (car p))))))]))
  (define (carnum? p) (num? (car p))))

(module main racket/base
  (require carnum?)
  (define top (carnum? •)))
