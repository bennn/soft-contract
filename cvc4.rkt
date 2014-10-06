#lang typed/racket/base
(provide cvc4)
(require racket/match racket/set racket/string racket/port racket/system
         "lib.rkt" "lang.rkt" "runtime.rkt" "provability.rkt")

(define-type/pred r² (U '+ '- '* '/ '> '< '≥ '≤ '= 'equal?))

(: cvc4 : -Γ -π′ → Derivability)
;; Query CVC4 for derivability of `π` from `Γ`
(define (cvc4 Γ π)
  (match (t π)
    [(? string? q)
     (define xs (string-join
                 (for/list : (Listof String) ([x (set-union (FV/Γ Γ) (FV/π π))])
                   (define X (list (+x x)))
                   (cond
                    [(equal? '✓ (Γ⊢ᵢ Γ (-π@ 'integer? X))) (format "~a : INT;" x)]
                    [(equal? '✓ (Γ⊢ᵢ Γ (-π@ 'real? X))) (format "~a : REAL;" x)]
                    [else (errorf "~a is not REAL" x)]))
                 "\n"
                 #:after-last "\n"))
     (define p (string-join
                (for*/list : (Listof String) ([π Γ] [s (in-value (t π))] #:when s)
                  (format "ASSERT ~a;" s))
                "\n"
                #:after-last "\n"))
     (call-with xs p q)]
    [_ '?]))

(: call-with : String String String → Derivability)
;; Perform quer(y|ies) with given declarations, assertions, and conclusion,
(define (call-with xs p q)
  (match (call (string-append xs p (format "QUERY ~a;~n" q)))
    [(regexp #rx"^valid") '✓]
    [(regexp #rx"^invalid")
     (match (call (string-append xs p (format "CHECKSAT ~a;~n" q)))
       [(regexp #rx"^unsat") 'X]
       [_ '?])]
    [_ '?]))

(: call : String → String)
;; Perform system call to CVC4 with given query
(define (call s)
  (printf "~nCVC4:~n~a~n" s)
  (with-output-to-string
      (λ () ; CVC4 from 1.3 no longer uses exit code to indicate status
        (system (format "echo \"~a\" | cvc4 -q" s)))))

(: t : -π′ → (Option String))
;; r² a pure expression into CVC4's expression
(define (t π)
  (: go (case->
         [-x → Symbol]
         [-b → Base]
         [-π@ → (Option String)]
         [-π → Any]))
  (define (go π)
    (match π
      [(-π@ (? r²? r) (list π₁ π₂)) (@? format "~a ~a ~a" (go π₁) (! (str r)) (go π₂))]
      [(-π@ 'add1 (list π)) (@? format "~a + 1" (go π))]
      [(-π@ 'sub1 (list π)) (@? format "~a - 1" (go π))]
      [(-π@ 'false? (list π))
       (@? format "NOT (~a)" (go π))]
      [(-π@ _ _) #f]
      [(-x x) x]
      [(-b b) (match b [#f 'FALSE] [#t 'TRUE] [b b])]))
  (match π
    ['ø #f]
    [(? -base?) #f]
    [(? -π@? π) (go π)]))

(: γ : -Γ → (Listof String))
;; r² an assumption environment into a list of CVC4 expressions
(define (γ Γ)
  (for*/list : (Listof String) ([π Γ] [s (in-value (t π))] #:when s) s))

(: str : r² → Symbol)
;; r² Racket symbol into CVC4 symbol
(define str
  (match-lambda ['equal? '=] [x x]))

