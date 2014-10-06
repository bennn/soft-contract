(module len racket/base
  (provide (contract-out [len ([l : (listof any)] . -> . (and/c int? (>=/c 0)))]))

  (define (len xs)
    (if (empty? xs) 0
        (+ 1 (len (cdr xs))))))

(module main racket/base
  (require len)
  (define top (len •)))
