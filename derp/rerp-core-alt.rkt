#lang racket/base
(require racket/match)
(provide (all-defined-out))

; Rerp Core implements the minimal core 
; of a regular language recognizer.

; Atomic languages:
(define-struct ∅     {}      #:transparent)  ; empty set
(define-struct ε     {}      #:transparent)  ; empty string
(define-struct token {value} #:transparent)  ; exact terminal

; Compound languages:
(define-struct ∪ {this that}  #:transparent) ; union
(define-struct ∘ {left right} #:transparent) ; concatenation
(define-struct ★ {lang}       #:transparent) ; repetition

; Derivative:
(define (D c L)
  (match L
    [(∅)           (∅)]
    [(ε)           (∅)]
    [(token a)     (cond [(eqv? a c) (ε)]
                         [else       (∅)])]

    [(∪ L1 L2)     (∪ (D c L1) 
                      (D c L2))]
    [(★ L1)        (∘ (D c L1) L)]
    [(∘ L1 L2)     (∪ (∘ (δ L1) (D c L2))
                      (∘ (D c L1) L2))]))

; Nullability:
(define (nullable? L)
  (match L
    [(∅)           #f]
    [(ε)           #t]
    [(token _)     #f]
    [(★ _)         #t]
    [(∪ L1 L2)     (or (nullable? L1) (nullable? L2))]
    [(∘ L1 L2)     (and (nullable? L1) (nullable? L2))]))

(define (δ L)
  (cond
    [(nullable? L)   (ε)]
    [else            (∅)]))

; Parse a list of tokens:
(define (recognizes? w L)
  (if (null? w)
      (nullable? L)
      (recognizes? (cdr w) (D (car w) L))))