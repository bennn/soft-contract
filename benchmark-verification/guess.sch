(module IO racket/base
  (provide
   (contract-out
    [display (str? . -> . any)]
    [error (str? . -> . any)]
    [read (-> any)])))

(module guess racket/base
  (provide (contract-out [guess (int? . -> . any)]))
  (require IO)
  (define (guess target)
    (let ([input (read)])
      (cond
        [(not (or (equal? input 'quit) (int? input)))
         (error "Invalid type for input")]
        [(equal? input 'quit) 'quit]
        [(< input target)
         (begin (display "Too low!\n") (guess target))]
        [(> input target)
         (begin (display "Too high!\n") (guess target))]
        [else (begin (display "Correct!\n") 'done)]))))

(module main racket/base
  (require guess)
  (define top (guess •)))
