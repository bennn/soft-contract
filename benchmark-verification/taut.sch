(module taut racket/base
  (provide (contract-out [taut ([μ/c (X) (or/c bool? [bool? . -> . X])] . -> . bool?)]))
  (define (taut b)
    (cond
      [(bool? b) b]
      [else (and (taut (b #t)) (taut (b #f)))])))

(module main racket/base
  (require taut)
  (define top (taut •)))
