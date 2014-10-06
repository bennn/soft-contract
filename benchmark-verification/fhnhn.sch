(module f racket/base
  (provide
   (contract-out
    [f ([x : (any . -> . int?)]
               . -> .
               ((and/c (any . -> . int?)
                       (λ (y) (not (and (> (x #f) 0) (< (y #f) 0))))) . -> . int?))])))

(module h racket/base
  (provide (contract-out [h (int? . -> . (any . -> . int?))]))
  (define (h x) (λ (_) x)))

(module g racket/base
  (provide (contract-out [g (int? . -> . int?)]))
  (require f h)
  (define (g n) ((f (h n)) (h n))))

(module main racket/base
  (provide (contract-out [main (int? . -> . int?)]))
  (require g)
  (define (main m) (g m)))

(module top racket/base
  (require main)
  (define top (main •)))
