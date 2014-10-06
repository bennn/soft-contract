(module filter racket/base ; opaque
  (provide
   (contract-out
    [filter ([p? : (any . -> . any)] [_ : (listof any)] . -> . (listof (λ (y) (p? y))))])))

(module add-nums racket/base
  (provide (contract-out [add-nums ((listof any) . -> . num?)]))
  (require filter)
  
  (define (add-nums xs)
    (foldl + 0 (filter num? xs)))
  
  (define (foldl f z xs)
    (if (empty? xs) z (foldl f (f (car xs) z) (cdr xs)))))

(module main racket/base
  (require add-nums)
  (define top (add-nums •)))
