(module map-foldr racket/base
  (provide
   (contract-out
    [foldr ((any any . -> . any) any (listof any) . -> . any)]
    [map ((any . -> . any) (listof any) . -> . (listof any))]))
  (define (foldr f z xs)
    (if (empty? xs) z
        (f (car xs) (foldr f z (cdr xs)))))
  (define (map f xs)
    (foldr (λ (x ys) (cons (f x) ys)) empty xs)))

(module main racket/base
  (require map-foldr)
  (define top (map • •)))
