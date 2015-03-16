#lang racket

(require (prefix-in z: "zombie.rkt"))
(require contract-profile
         "zombie.rkt")

(define z:h (reverse (with-input-from-file "zombie-hist-4.txt" read)))

(define (nocontract-main)
  (displayln "in the thunk")
  (z:replay z:unsafe:w1 z:h))

(contract-profile-thunk nocontract-main)
