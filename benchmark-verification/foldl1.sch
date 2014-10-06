(module foldl1 racket/base
  (provide (contract-out [foldl1 ((any any . -> . any) (nelistof any) . -> . any)]))
  (define (foldl1 f xs)
    (let ([z (car xs)]
          [zs (cdr xs)])
      (if (empty? zs) z
          (foldl1 f (cons (f z (car zs)) (cdr zs)))))))

(module main racket/base
  (require foldl1)
  (define top (foldl1 • •)))
