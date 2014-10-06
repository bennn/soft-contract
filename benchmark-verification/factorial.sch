(module factorial racket/base
  (provide (contract-out [factorial (int? . -> . int?)]))
  (define (factorial n)
    (if (zero? n) 1 (* n (factorial (sub1 n))))))

(module main racket/base
  (require factorial)
  (define top (factorial •)))
