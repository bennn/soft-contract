(module ack racket/base
  (provide
   (contract-out
    [ack (integer? integer? . -> . integer?)]))
  (define (ack m n)
    (cond
      [(= m 0) (+ n 1)]
      [(= n 0) (ack (- m 1) 1)]
      [else (ack (- m 1) (ack m (- n 1)))])))

(module main racket/base
  (require ack)
  (define top (ack • •)))
