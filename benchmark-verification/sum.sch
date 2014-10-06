(module sum racket/base
  (provide (contract-out [sum ([n : int?] . -> . (and/c int? (>=/c n)))]))
  (define (sum n)
    (if (<= n 0) 0
        (+ n (sum (- n 1))))))

(module main racket/base
  (require sum)
  (define top (sum •)))
