(module member racket/base
  (provide (contract-out [member (any (listof any) . -> . (listof any))]))
  (define (member x l)
    (if (empty? l) empty
        (if (equal? x (car l)) l (member x (cdr l))))))

(module main racket/base
  (require member)
  (define top (member • •)))
