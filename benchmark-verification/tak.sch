(module tak racket/base
  (provide (contract-out [tak (int? int? int? . -> . int?)]))
  (define (tak x y z)
    (if (false? (< y x)) z
        (tak (tak (- x 1) y z)
             (tak (- y 1) z x)
             (tak (- z 1) x y)))))
(module nums racket/base
  (provide (contract-out [a int?] [b int?] [c int?])))

(module main racket/base
  (require tak nums)
  (define top (tak a b c)))
