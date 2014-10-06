(module rand racket/base (provide (contract-out (rand (-> int?)))))

(module fold-div racket/base
  (provide
   (contract-out
    [foldl ((real? real? . -> . real?) real? (listof real?) . -> . real?)]
    [randpos (-> int?)]
    [mk-list (int? . -> . (listof (and/c int? positive?)))]
    [main (int? int? . -> . real?)]))
  (require rand)
  (define (foldl f z l)
    (if (empty? l) z (foldl f (f z (car l)) (cdr l))))
  (define (randpos)
    (let [n (rand)] (if (> n 0) n (randpos))))
  (define (mk-list n)
    (if (<= n 0) empty
        (cons (randpos) (mk-list (- n 1)))))
  (define (main n m) (foldl / m (mk-list n))))

(module main racket/base
  (require fold-div)
  (define top (main • •)))
