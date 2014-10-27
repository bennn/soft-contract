(module tree
  (provide
   [braun-tree? (any . -> . bool?)]
   [insert (braun-tree? any . -> . braun-tree?)])
  
  (struct node (v l r))

  (define (braun-tree?/helper x)
    (cond
     [(false? x) (cons #t 0)]
     [(node? x)
      (let ([L (braun-tree?/helper (node-l x))])
        (cond
         [(car L)
          (let ([R (braun-tree?/helper (node-r x))])
            (cond
             [(car R)
              (let ([l# (cdr L)]
                    [r# (cdr R)])
                (cons (or (= l# r#) (= l# (add1 r#)))
                      (add1 (+ l# r#))))]
             [else (cons #f 0)]))]
         [else (cons #f 0 #|doesn't matter|#)]))]
     [else (cons #f 0 #|doesn't matter|#)]))
  
  (define (braun-tree? x)
    (car (braun-tree?/helper x))
    #;(or (false? x)
    (and (node? x)
    (braun-tree? (node-l x))
    (braun-tree? (node-r x))
    (let ([l (size (node-l x))]
    [r (size (node-r x))])
    (or (= l r) (= l (add1 r)))))))
  
  (define (size x)
    (if (node? x)
        (add1 (+ (size (node-l x)) (size (node-r x))))
        0))
  
  (define (insert bt x)
    (if (node? bt)
        (node (node-v bt) (insert (#|HERE|#node-l bt) x) (node-r bt))
        (node x #f #f))))

(require tree)
(insert • 42)

