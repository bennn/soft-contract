(module isnil racket/base
  (provide
   (contract-out
    [mk-list ([n : int?] . -> .
                     (and/c (listof int?)
                            (λ (l) (implies (> n 0) (cons? l)))))]))
  (define (mk-list n)
    (if (= n 0) empty (cons n (mk-list (- n 1))))))

(module main racket/base
  (require isnil)
  (define top (mk-list •)))
