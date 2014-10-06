(module tree-depth racket/base
  (provide
   (contract-out
    [depth (TREE/C . -> . (and/c int? (>=/c 0)))]
    [TREE/C any]))
  (struct leaf ())
  (struct node (l r))
  (define (depth t)
    (if (node? t) (+ 1 (max (depth (node-l t)) (depth (node-r t)))) 0))
  (define (max x y) (if (> x y) x y))
  (define TREE/C (μ/c (X) (or/c (struct/c leaf) (struct/c node X X)))))

(module main racket/base
  (require tree-depth)
  (define top (depth •)))
