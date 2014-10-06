(module lib racket/base
  (provide (contract-out [string-append (str? str? . -> . str?)])))

(module inc-or-greet racket/base
  (provide (contract-out [inc-or-greet (bool? (or/c str? int?) . -> . (or/c false? int? str?))]))
  (require lib)
  (define (inc-or-greet mode y)
    (if mode
        (if (int? y) (+ y 1) #f)
        (if (str? y) (string-append "Hello" y) #f))))

(module main racket/base
  (require inc-or-greet)
  (define top (inc-or-greet • •)))
