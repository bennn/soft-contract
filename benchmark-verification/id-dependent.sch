(module opaque racket/base
  (provide (contract-out [n num?])))

(module id racket/base
  (provide (contract-out [f ([x : num?] . -> . (=/c x))]))
  (define (f x) x))

(module main racket/base
  (require opaque id)
  (define top (f n)))
