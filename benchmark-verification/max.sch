(module max racket/base
  (provide (contract-out [max ((int? int? . -> . int?) int? int? int? . -> . int?)]))
  (define (max max2 x y z) (max2 (max2 x y) z)))

(module f racket/base
  (provide (contract-out [f (int? int? . -> . int?)]))
  (define (f x y) (if (>= x y) x y)))

(module main racket/base
  (provide
   (contract-out
    [main ([x : int?] [y : int?] [z : int?]
	   . -> . (and/c int? (λ (m) (= (f x m) m))))]))
  (require max f)
  (define (main x y z)
    (max f x y z)))

(module top racket/base
  (require main)
  (define top (main • • •)))
