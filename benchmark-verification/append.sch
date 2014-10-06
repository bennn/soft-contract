(module append racket/base
  (provide
   (contract-out
    [append ((listof any) (listof any) . -> . (listof any))]))
  (define (append xs ys)
    (if (empty? xs) ys
        (cons (car xs) (append (cdr xs) ys)))))

(module main racket/base
  (require append)
  (define top (append • •)))
