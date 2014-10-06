(module hrec racket/base
  (provide
   (contract-out
    [f ((int? . -> . int?) int? . -> . int?)]
    [main (int? . -> . (and/c int? (>=/c 0)))]))
  (define (f g x)
    (if (>= x 0) (g x) (f (λ (x) (f g x)) (g x))))
  (define (main n)
    (f add1 n)))

(module main racket/base
  (require hrec)
  (define top (main •)))
