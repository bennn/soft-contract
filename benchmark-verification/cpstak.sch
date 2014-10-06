(module tak racket/base
  (provide
   (contract-out [tak (int? int? int? (int? . -> . int?) . -> . int?)]))
  (define (tak x y z k)
    (if (not (< y x))
        (k z)
        (tak (- x 1)
             y
             z
             (lambda (v1)
               (tak (- y 1)
                    z
                    x
                    (lambda (v2)
                      (tak (- z 1)
                           x
                           y
                           (lambda (v3)
                             (tak v1 v2 v3 k))))))))))

(module nums racket/base
  (provide (contract-out [a int?] [b int?] [c int?])))

(module main racket/base
  (require tak nums)
  (define top (tak a b c (lambda (x) x))))
