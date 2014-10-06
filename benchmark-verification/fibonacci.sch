(module fib racket/base
  (provide (contract-out [fib (int? . -> . int?)]))
  (define (fib n)
    (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))))

(module main racket/base
  (require fib)
  (define top (fib •)))
