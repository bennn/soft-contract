(module map-append racket/base
  (provide
   (contract-out
    [map-append ((any . -> . (listof any)) (listof any) . -> . (listof any))]
    [append ((listof any) (listof any) . -> . (listof any))]))
  
  (define (append xs ys)
    (if (empty? xs) ys
        (cons (car xs) (append (cdr xs) ys))))
  
  (define (map-append f xs)
    (if (empty? xs) empty
        (append (f (car xs)) (map-append f (cdr xs))))))

(module main racket/base
  (require map-append)
  (define top (map-append • •)))
