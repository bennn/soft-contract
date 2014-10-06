(module g racket/base
  (provide (contract-out [g (int? . -> . (any . -> . int?))]))
  (define (g x) (λ (_) x)))

(module twice racket/base
  (provide
   (contract-out
    [twice (((any . -> . int?) . -> . (any . -> . int?)) (any . -> . int?) any . -> . int?)]))
  (define (twice f x y) ((f (f x)) y)))

(module neg racket/base
  (provide (contract-out [neg ((any . -> . int?) . -> . (any . -> . int?))]))
  (define (neg x) (λ (_) (- 0 (x #f)))))

(module main racket/base
  (provide (contract-out [main (int? . -> . (and/c int? (>=/c 0)))]))
  (require twice neg g)
  (define (main n)
    (if (>= n 0)
        (twice neg (g n) 'unit)
        42)))

(module top racket/base
  (require main)
  (define top (main •)))
