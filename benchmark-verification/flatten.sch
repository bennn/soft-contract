(module lib racket/base
  (provide (contract-out [append ((listof any) (listof any) . -> . (listof any))])))

(module flatten racket/base
  (provide (contract-out [flatten (any . -> . (listof any))]))
  (require lib)
  (define (flatten x)
    (cond [(empty? x) empty]
	  [(cons? x) (append [flatten (car x)] [flatten (cdr x)])]
	  [else (cons x empty)])))

(module main racket/base
  (require flatten)
  (define top (flatten •)))
