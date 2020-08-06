#lang racket/base
(require racket/match
         racket/promise
         "util-xp.rkt")
(provide (all-defined-out)
         (struct-out rec))

; Herp Core implements the minimal core
; of a context-free language recognizer.

; Atomic languages:
(struct ∅     {}      #:transparent)  ; empty set
(struct ε     {}      #:transparent)  ; empty string
(struct token {value} #:transparent)  ; exact terminal

; Compound languages:
(struct δ {lang} #:transparent)       ; nullability
(struct ∪ {this that} #:transparent)  ; union
(struct ∘ {left right} #:transparent) ; concatenation
(struct ★ {lang} #:transparent)       ; repetition

;; Recursive languages:
;; (struct rec (p)) defined in util-xp.rkt

; Derivative:

(define (D c L)
  (define h (make-hash))
  (define (loop L)
    (match L
      [(∅)           (∅)]
      [(ε)           (∅)]
      [(δ _)         (∅)]
      [(token a)     (cond [(eqv? a c) (ε)]
                           [else       (∅)])]

      [(∪ L1 L2)     (∪ (loop L1)
                        (loop L2))]
      [(★ L1)        (∘ (loop L1) L)]
      [(∘ L1 L2)     (∪ (∘ (δ L1) (loop L2))
                        (∘ (loop L1) L2))]

      [(rec p)       (let ([p* (hash-ref! h L (delay (loop (force p))))])
                       (rec p*))]))
  (loop L))


; Nullability:
(define nullable?
  (fixed-point
   #:bottom #f #:top #t
   (lambda (nullable?)
     (lambda (L)
       (match L
         [(∅)           #f]
         [(ε)           #t]
         [(token _)     #f]
         [(★ _)         #t]
         [(δ L1)        (nullable? L1)]
         [(∪ L1 L2)     (or (nullable? L1) (nullable? L2))]
         [(∘ L1 L2)     (and (nullable? L1) (nullable? L2))]
         ;; [(rec pL) ...] handled by fixed-point
         )))))


; Parse a list of tokens:
(define (recognizes? w L)
  (nullable? (foldl D L w)))
