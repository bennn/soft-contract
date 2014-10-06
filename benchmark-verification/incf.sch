(module obj racket/base
  (provide
   (contract-out
    [alloc (-> (none/c . -> . any))]
    [update ((any . -> . any) any any . -> . (any . -> . any))]
    [select ((any . -> . any) any . -> . any)]))
  (define (alloc) (λ (_) _))
  (define (update f k v)
    (λ (x) (if (equal? x k) v (f k))))
  (define (select f x) (f x)))

(module assert racket/base
  (provide (contract-out [assert ((not/c false?) . -> . any)])))

;; translated from Swamy et al. 2013
(module main racket/base
  (provide (contract-out [main ((any . -> . any) . -> . (any . -> . any))]))
  (require obj assert)
  (define (main global)
    (let* ([incf (λ (this args)
                   (let ([x (select args "0")])
                     (update x "f" (+ 1 (select x "f")))))]
           [global (update global "incf" incf)]
           [args (let ([x (update (alloc) "f" 0)])
                   (update (alloc) "0" x))]
           [res ((select global "incf") global args)])
      (begin
        (assert (= (select res "f") 1))
        (let ([global (update global "incf" 0)])
          global)))))

(module top racket/base
  (require main)
  (define top (main •)))
