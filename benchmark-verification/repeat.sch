(module repeat racket/base
  (provide
   (contract-out
    [repeat ([f : (any . -> . any)] [n : int?] [s : any]
	     . -> . (λ (a) (implies (= n 0) (equal? a s))))]))
  (define (repeat f n s)
    (if (= n 0) s (f (repeat f (- n 1) s)))))

(module main racket/base
  (require repeat)
  (define top (repeat • • •)))
