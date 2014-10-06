(module factorial racket/base
  (provide (contract-out [factorial (int? . -> . int?)]))
  (define (factorial n)
    (factorial-acc n 1))
  (define (factorial-acc n acc)
    (if (zero? n) acc
        (factorial-acc (sub1 n) (* n acc)))))

(module main racket/base
  (require factorial)
  (define top (factorial •)))
