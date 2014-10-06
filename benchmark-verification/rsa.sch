(module keygen racket/base
  (require prime?)
  (provide (contract-out [keygen (any . -> . (λ (x) (prime? x)))])))

(module rsa racket/base
  (require prime?)
  (provide (contract-out [rsa ((λ (x) (prime? x)) any . -> . any)])))

(module prime? racket/base
  (provide (contract-out [prime? (any . -> . any)])))

(module enc racket/base
  (provide (contract-out [enc (any . -> . any)]))
  (require rsa keygen)
  (define (enc x) (rsa (keygen #t) x)))

(module main racket/base
  (require enc)
  (define top (enc •)))
