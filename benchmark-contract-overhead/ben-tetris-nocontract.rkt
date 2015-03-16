#lang racket  

(require (prefix-in t: "tetris.rkt"))
(require contract-profile)

(define t:hist (reverse (with-input-from-file "tetris-hist-3.txt" read)))

(define (nocontract-main)
  (t:unsafe:replay t:unsafe:w0 t:hist))

(contract-profile-thunk nocontract-main)
