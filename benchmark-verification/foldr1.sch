(module foldr1 racket/base
  (provide (contract-out [foldr1 ((any any . -> . any) (nelistof any) . -> . any)]))
  (define (foldr1 f xs)
    (let ([z (car xs)]
          [zs (cdr xs)])
      (if (empty? zs) z
          (f z (foldr1 f zs))))))

(module main racket/base
  (require foldr1)
  (define top (foldr1 • •)))
