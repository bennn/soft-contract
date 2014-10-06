(module dvh-1 racket/base
  (provide
   (contract-out [main ([z : (and/c num? (=/c 5))] . -> . (=/c z))]))

  (define (main x) (- (+ x x) x)))

(module main racket/base
  (require dvh-1)
  (define top (main •)))
