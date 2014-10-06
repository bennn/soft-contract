(module bt racket/base
  (provide
   (contract-out
    [num (NODE/C . -> . num?)]
    [left (NODE/C . -> . BT/C)]
    [right (NODE/C . -> . BT/C)]
    [BT/C any]
    [NODE/C any]))
  (define num car)
  (define (left node) (car (cdr node)))
  (define (right node) (cdr (cdr node)))
  (define BT/C (μ/c (X) (or/c num? (cons/c num? (cons/c X X)))))
  (define NODE/C (cons/c num? (cons/c BT/C BT/C))))

(module sum racket/base
  (provide (contract-out [sum (BT/C . -> . num?)]))
  (require bt)
  (define (sum t)
    (if (num? t) t
        (+ (num t) (+ (sum (left t)) (sum (right t)))))))

(module map racket/base
  (provide (contract-out [map ((num? . -> . num?) BT/C . -> . BT/C)]))
  (require bt)
  (define (map f t)
    (if (num? t) (f t)
        (cons (f (num t))
              (cons (map f (left t)) (map f (right t)))))))

(module t racket/base
  (provide (contract-out [t BT/C])) (require bt))

(module main racket/base
  (require sum map t)
  (define top (amb (sum t) (map add1 t))))
