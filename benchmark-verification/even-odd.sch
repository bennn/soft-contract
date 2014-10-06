(module eo racket/base
  (provide
   (contract-out [even? (int? . -> . bool?)]
		 [odd? (int? . -> . bool?)]))
  (define (even? n)
    (if (zero? n) #t (odd? (sub1 n))))
  (define (odd? n)
    (if (zero? n) #f (even? (sub1 n)))))

(module main racket/base
  (require eo)
  (define top (even? •)))
