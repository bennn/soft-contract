(module foldl racket/base
  (provide (contract-out [foldl ((any any . -> . any) any (listof any) . -> . any)]))
  (define (foldl f z xs)
    (if (empty? xs) z
        (foldl f (f (car xs) z) (cdr xs)))))

(module main racket/base
  (require foldl)
  (define top (foldl • • •)))
