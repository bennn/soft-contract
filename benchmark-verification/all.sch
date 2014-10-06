(module all racket/base
  (provide
   (contract-out
    [all ((any . -> . any) (listof any) . -> . any)]))
  (define (all p? xs)
    (cond
      [(empty? xs) #t]
      [(empty? (cdr xs)) (p? (car xs))]
      [else (and (p? (car xs)) (all p? (cdr xs)))])))

(module main racket/base
  (require all)
  (define top (all • •)))
