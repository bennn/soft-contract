(module foldr racket/base
  (provide (contract-out [foldr ((any any . -> . any) any (listof any) . -> . any)]))
  (define (foldr f z xs)
    (if (empty? xs) z
        (f (car xs) (foldr f z (cdr xs))))))

(module main racket/base
  (require foldr)
  (define top (foldr • • •)))
