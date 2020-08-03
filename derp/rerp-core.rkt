#lang racket/base
(require racket/match "util.rkt")
(provide (all-defined-out))

; Rerp Core implements the minimal core 
; of a regular language recognizer.

; Atomic languages:
(struct ∅     {}      #:transparent)  ; empty set
(struct ε     {}      #:transparent)  ; empty string
(struct token {value} #:transparent)  ; exact terminal

; Compound languages:
(struct ∪ {this that}  #:transparent) ; union
(struct ∘ {left right} #:transparent) ; concatenation
(struct ★ {lang}       #:transparent) ; repetition
(struct δ {lang}       #:transparent) ; nullability

; Derivative:
(define (D c L)
  (match L
    [(∅)           (∅)]
    [(ε)           (∅)]
    [(δ _)         (∅)]
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
    [(δ L1)        (nullable? L1)]
    [(∪ L1 L2)     (or (nullable? L1) (nullable? L2))]
    [(∘ L1 L2)     (and (nullable? L1) (nullable? L2))]))


; Parse a list of tokens:
(define (recognizes? w L)
  (if (null? w)
      (nullable? L)
      (recognizes? (cdr w) (D (car w) L))))
