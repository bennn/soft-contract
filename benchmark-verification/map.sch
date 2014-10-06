(module map racket/base
  (provide
   (contract-out
    [map ([_ : (any . -> . any)] [l : (listof any)] . -> .
                                (and/c (listof any)
                                       (λ (r) (equal? (empty? l) (empty? r)))))]))
  (define (map f xs)
    (if (empty? xs) empty
        (cons (f (car xs)) (map f (cdr xs))))))

(module main racket/base
  (require map)
  (define top (map • •)))
