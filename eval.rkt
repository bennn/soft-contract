#lang typed/racket/base
(require racket/match racket/set
         "abbrevs.rkt" "lib.rkt" "lang.rkt" "parser.rkt" "runtime.rkt" "provability.rkt" "step.rkt" "cvc4.rkt")

(: run : Any #|Sexp|# → (ND Any))
(define (run s)
  (define Σ₀ (inj (parse s)))
  (define-set ans : -Σ/final)
  
  (let visit : (ND Void) ([Σ : -Σ/kont Σ₀])
    (let/nd : (-Σ → Void) ([Σ′ (↦ₚ Σ)])
      (cond [(-Σ/kont? Σ′) (visit Σ′)]
            [else (ans-add! Σ′)])))
  (printf "~a result(s)~n" (set-count ans))
  ans)

(: interact : → Void)
;; Just for interactive debugging
(define (interact)
  (printf "eval> ")
  (printf "~a~n~n"
          (run `((module main racket/base
                   #;(require arith)
                   (define opq ●)
                   (define top ,(read))))))
  (interact))

(parameterize ([Γ⊢ₓ cvc4])
  (interact))
