#lang racket  

(require (prefix-in s: "snake.rkt"))
(require contract-profile)

(define s:h4 (reverse (with-input-from-file "snake-hist-4.txt" read)))

(define (nocontract-main)
  (s:unsafe:replay s:unsafe:w0 s:h4))

(contract-profile-thunk nocontract-main)
