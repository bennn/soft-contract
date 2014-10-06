(module lock racket/base
  (provide
   (contract-out
    [lock ((one-of/c 0) . -> . (one-of/c 1))]
    [unlock ((one-of/c 1) . -> . (one-of/c 0))]))
  (define (lock st) 1)
  (define (unlock st) 0))

(module fg racket/base
  (provide
   (contract-out
    [f (int? int? . -> . int?)]
    [g (int? int? . -> . int?)]))
  (require lock)
  (define (f n st) (if (> n 0) (lock st) st))
  (define (g n st) (if (> n 0) (unlock st) st)))

(module main racket/base
  (provide (contract-out [main (int? . -> . (one-of/c 0))]))
  (require fg)
  (define (main n) (g n (f n 0))))

(module top racket/base
  (require main)
  (define top (main •)))
