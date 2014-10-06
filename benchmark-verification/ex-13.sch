(module ex-13 racket/base
  (provide (contract-out [f (any any . -> . true?)]))
  (define (f x y)
    (cond
      [(and (num? x) (str? y)) (and (num? x) (str? y))]
      [(num? x) (and (num? x) (not (str? y)))]
      [else (not (num? x))])))

(module main racket/base
  (require ex-13)
  (define top (f • •)))
