(module mc91 racket/base
  (provide
   (contract-out [mc91 ([n : int?] . -> . (and/c int? (λ (a) (implies (<= n 101) (= a 91)))))]))
  (define (mc91 x)
    (if (> x 100) (- x 10)
        (mc91 (mc91 (+ x 11))))))

(module main racket/base
  (require mc91)
  (define top (mc91 •)))
