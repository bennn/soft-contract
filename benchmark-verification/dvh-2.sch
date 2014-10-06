(module dvh-2 racket/base
  (provide
   (contract-out [main ([x : num?] . -> . ([y : (and/c num? (=/c x))] . -> . (=/c x)))]))

  (define (main x) (lambda (y) y)))

(module main racket/base
  (require dvh-2)
  (define top ((main •) •)))
