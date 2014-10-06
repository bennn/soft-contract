(module opaque racket/base
  (provide
   (contract-out
    [insert (int? SORTED/C . -> . (and/c (nelistof int?) ne-sorted?))]
    [ne-sorted? ((nelistof int?) . -> . bool?)]
    [SORTED/C any]))
  (define SORTED/C (or/c empty? (and/c (nelistof int?) ne-sorted?))))

(module insertion-sort racket/base
  (require opaque)
  (provide
   (contract-out
    [sort ([l : (listof int?)] . -> . (and/c SORTED/C (λ (r) (if (empty? l) (empty? r) (cons? r)))))]))
  (define (sort xs) (foldl insert xs empty))
  (define (foldl f l b)
    (if (empty? l) b (foldl f (cdr l) (f (car l) b)))))

(module main racket/base
  (require insertion-sort)
  (define top (sort •)))
