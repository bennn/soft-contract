(module len racket/base
  (provide (contract-out [len ([l : (listof any)] . -> . (and/c int? (>=/c 0)))]))
  (define (len xs)
    (len-acc xs 0))
  (define (len-acc xs acc)
    (if (empty? xs) acc
        (len-acc (cdr xs) (+ 1 acc)))))

(module main racket/base
  (require len)
  (define top (len •)))
