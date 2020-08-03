#lang racket/base
(require racket/match
         "util.rkt")
(provide (all-defined-out))

; Herp Core implements the minimal core
; of a context-free language recognizer.

; Atomic languages:
(struct ∅     {}      #:transparent)  ; empty set
(struct ε     {}      #:transparent)  ; empty string
(struct token {value} #:transparent)  ; exact terminal

; Compound languages:
(lazy-struct δ {lang})       ; nullability
(lazy-struct ∪ {this that})  ; union
(lazy-struct ∘ {left right}) ; concatenation
(lazy-struct ★ {lang})       ; repetition

; Derivative:
(define/memoize (D c L)
  #:order ([L #:eq] [c #:equal])
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
(define/fix (nullable? L)
  #:bottom #f
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
