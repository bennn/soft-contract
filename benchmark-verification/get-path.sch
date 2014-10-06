(module lib racket/base
  (provide
   (contract-out
    [path/c any]
    [dom/c any]))
  (define path/c
    ([msg : (one-of/c "hd" "tl")] . -> .
     (cond
       [(equal? msg "hd") str?]
       [else (or/c false? path/c)])))
  (define dom/c
    ([msg : (one-of/c "get-child")] . -> .
     (cond
       [(equal? msg "get-child") (str? . -> . dom/c)]
       [else false?]))))

(module get-path racket/base
  (provide (contract-out [get-path (dom/c path/c . -> . dom/c)]))
  (require lib)
  (define (get-path root p)
    (while root p))
  (define (while cur path)
    (if (and (not (false? path)) (not (false? cur)))
        (while ((cur "get-child") (path "hd"))
               (path "tl"))
        cur)))

(module main racket/base
  (require get-path)
  (define top (get-path • •)))
