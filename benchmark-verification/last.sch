(module Y racket/base
  (provide
   (contract-out
    [Y (([any . -> . any] . -> . [any . -> . any]) . -> . [any . -> . any])]))
  (define (Y f)
    (λ (y)
      (((λ (x) (f (λ (z) ((x x) z))))
        (λ (x) (f (λ (z) ((x x) z)))))
       y))))

(module last racket/base
  (require Y)
  (provide (contract-out [last ((cons/c any (listof any)) . -> . any)]))
  (define (last l)
    ((Y (λ (f)
          (λ (x)
            (if (empty? (cdr x)) (car x) (f (cdr x))))))
     l)))

(module main racket/base
  (require last)
  (define top (last •)))
