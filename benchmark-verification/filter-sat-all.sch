(module filter-dep racket/base
  (provide
   (contract-out
    [filter ([p? : (any . -> . any)] [xs : (listof any)] . -> . (listof (λ (x) (p? x))))]))
  (define (filter p? xs)
    (cond
      [(empty? xs) empty]
      [else (let ([x (car xs)]
                  [zs (filter p? (cdr xs))])
              (if (p? x) (cons x zs) zs))])))

(module main racket/base
  (require filter-dep)
  (define top (filter • •)))
