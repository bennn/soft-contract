#lang typed/racket/base
(provide
 ;; Syntax
 define-type/pred
 ;; Types
 Map Map* ℕ ℕ? ℤ ℤ? ℝ ℝ? ℂ ℂ? 𝔹 𝔹? NeListof ND
 ;; Values
 ∅)

(require racket/set (for-syntax racket/base racket/syntax))

(define-syntax (define-type/pred stx)
  (syntax-case stx ()
    [(_ τ e) (with-syntax ([τ? (format-id #'τ "~a?" #'τ)])
               #'(begin (define-type τ e)
                        (define-predicate τ? τ)))]))

;; Type abbreviations
(define-type/pred ℕ Natural)
(define-type/pred ℤ Integer)
(define-type/pred ℝ Real)
(define-type/pred ℂ Number)
(define-type/pred 𝔹 Boolean)
(define-type Map HashTable)
(define-type (Map* X Y) (Map X (Setof Y)))
(define-type (NeListof X) (Pairof X (Listof X)))
(define-type (ND τ) (U τ (Setof τ)))

;; Value abbreviations
(define ∅ : (Setof Nothing) {set})
