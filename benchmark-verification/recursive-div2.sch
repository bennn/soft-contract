(module recursive-div2 racket/base
  (provide
   (contract-out
    [recursive-div2 ((μ/c (X) (or/c empty? (cons/c any (cons/c any X))))
		     . -> . (listof any))]))
  (define (recursive-div2 l)
    (if (empty? l) empty
        (cons (car l) (recursive-div2 (cdr (cdr l)))))))

(module main racket/base
  (require recursive-div2)
  (define top (recursive-div2 •)))
