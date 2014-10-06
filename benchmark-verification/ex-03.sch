(module lib racket/base
  (provide (contract-out [member (any (listof any) . -> . (or/c false? (nelistof any)))])))
(module ex-03 racket/base
  (provide (contract-out [f (any (listof any) . -> . false?)]))
  (require lib)
  (define (f v l)
    (let ([x (member v l)])
      (if x (false? f) #f))))
(module main racket/base
  (require ex-03 lib)
  (define top (f • •)))
