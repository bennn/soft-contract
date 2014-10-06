(module f 4
  (provide (contract-out [f ([or/c num? str?] cons? . -> . num?)]))
  (define (f input extra)
    (cond
      [(and (num? input) (num? (car extra)))
       (+ input (car extra))]
      [(num? (car extra))
       (+ (str-len input) (car extra))]
      [else 0])))

(module main racket/base
  (require f)
  (define top (f • •)))
