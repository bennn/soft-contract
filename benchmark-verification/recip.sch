(module recip racket/base
  (provide
   (contract-out
    [recip (any . -> . (or/c (and/c num? (not/c zero?)) str?))]))
  (define (recip x)
    (if (and (num? x) (not (zero? x)))
        (/ 1 x)
        "expect non-zero number")))

(module main racket/base
  (require recip)
  (define top (recip •)))
