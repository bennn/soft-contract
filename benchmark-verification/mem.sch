(module mem racket/base
  (provide
   (contract-out
    [mk-list ([_ : int?] [x : int?] . -> . (and/c (listof int?)
                                                 (λ (l) (or (empty? l) (mem x l)))))]
    [mem (int? (listof int?) . -> . bool?)]))
  (define (mk-list n x)
    (if (< n 0) empty (cons x (mk-list (- n 1) x))))
  (define (mem x xs)
    (if (empty? xs) #f (or (= x (car xs)) (mem x (cdr xs))))))

(module main racket/base
  (require mem)
  (define top (mk-list • •)))
