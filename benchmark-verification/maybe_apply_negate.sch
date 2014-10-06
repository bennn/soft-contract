(module negate racket/base
  (provide (contract-out [negate ((or/c int? bool?) . -> . (or/c int? bool?))]))
  (define (negate x)
    (if (int? x) (- 0 x) (not x))))

(module maybe-apply racket/base
  (provide (contract-out [maybe-apply (int? (or/c false? (int? . -> . int?)) . -> . int?)]))
  (define (maybe-apply x f)
    (if (false? f) x (f x))))

(module opaque racket/base (provide (contract-out [n int?])))

(module main racket/base
  (provide (contract-out [main (-> int?)]))
  (require maybe-apply negate opaque)
  (define (main)
    (maybe-apply n negate)))

(module top racket/base
  (require negate maybe-apply main)
  (define top (amb (negate •) (maybe-apply • •) (main))))
