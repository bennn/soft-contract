(module reverse-dep racket/base
  (provide
   (contract-out
    [reverse ((xs : (listof any))
             . -> .
             (and/c (listof any)
                    (λ (ys) (equal? (empty? xs) (empty? ys)))))]
    [append ((listof any) (listof any) . -> . (listof any))]))
  (define (append xs ys)
    (if (empty? xs) ys
        (cons (car xs) (append (cdr xs) ys))))
  (define (reverse xs)
    (if (empty? xs) empty
        (append (cdr xs) (cons (car xs) empty)))))

(module main racket/base
  (require reverse-dep)
  (define top (reverse •)))
