(module recip racket/base
  (provide
   (contract-out
    [recip (non-zero/c . -> . non-zero/c)]
    [non-zero/c any]))
  (define (recip x) (/ 1 x))
  (define non-zero/c (and/c num? (not/c zero?))))

(module main racket/base
  (require recip)
  (define top (recip •)))
