#lang racket/base
(require racket/match
         racket/promise
         racket/set
         (except-in "derp-core-xr.rkt" ∅? ε?)
         "util-xr.rkt")
(provide (all-defined-out))

; Nullability:
(define nullable?
  (fixed-point
   #:bottom #f #:top #t
   (lambda (nullable?)
     (lambda (L)
       (match L
         [(∅)        #f]
         [(ε _)      #t]
         [(token _)  #f]
         [(δ L)      (nullable? L)]
         [(∪ L1 L2)  (or  (nullable? L1) (nullable? L2))]
         [(∘ L1 L2)  (and (nullable? L1) (nullable? L2))]
         [(★ _)      #t]
         [(→ L1 _)   (nullable? L1)]
         ;;[(rec p) ...] handled by fixed-point
         )))))

;; Equal to the null language (ε):
(define ε?
  (fixed-point
   #:bottom #t #:top #f
   (lambda (ε?)
     (lambda (L)
       (match L
         [(∅)           #f]
         [(ε _)         #t]
         [(token _)     #f]
         [(δ L)         (nullable? L)]
         [(∪ L1 L2)     (and (ε? L1) (ε? L2))]
         [(∘ L1 L2)     (and (ε? L1) (ε? L2))]
         [(★ L1)        (or  (ε? L1) (∅? L1))]
         [(→ L1 _)      (ε?  L1)]
         ;;[(rec p) ...] handled by fixed-point
         )))))

; Compute the size of a set:
(define (set-choose s)
  (define el #f)
  (for ([el* s])
    (set! el el*))
  el)

; Matches a language if it is *exactly* the empty string:
(define-match-expander nullp
  (syntax-rules ()
    [(_)    (? ε?)]
    [(_ el) (and (? ε?)
                 (app parse-null (and (app set-count 1)
                                      (app set-choose el))))]))

; Checks whether a language is the empty set:
(define ∅?
  (fixed-point
   #:bottom #t #:top #f
   (lambda (∅?)
     (lambda (L)
       (match L
         [(∅)         #t]
         [(ε _)       #f]
         [(token _)   #f]
         [(δ L)       (not (nullable? L))]
         [(★ L1)      #f]
         [(∪ L1 L2)   (and (∅? L1) (∅? L2))]
         [(∘ L1 L2)   (or  (∅? L1) (∅? L2))]
         [(→ L1 _)    (∅?  L1)]
         ;;[(rec p) ...] handled by fixed-point
         )))))

; Optimizing compaction.
; (K L) is an equivalent, compacted version of L.
(define K
  (rec-memoize
   (lambda (K)
     (lambda (L)
       (define (unrec v) (match v [(rec pp) pp] [_ v]))
       (define-match-expander I (syntax-rules () [(I p) (app unrec p)]))
       #;(define-match-expander I (syntax-rules () [(I p) p]))
       (match L
         [(∅)         L]
         [(ε _)       L]
         [(? ∅?)      (∅)]
         [(? ε?)      (ε (parse-null L))]
         [(token _)   L]

         [(★ (? ∅?))    (ε (set '()))]
         [(★ L)         (★ (K L))]

         [(∪ (? ∅?) L2)   (K L2)]
         [(∪ L1 (? ∅?))   (K L1)]
         [(∪ L1 L2)   (∪ (K L1) (K L2))]

         [(∘ (nullp t) L2)   (→ (K L2) (λ (w2) (cons t w2)))]
         [(∘ L1 (nullp t))   (→ (K L1) (λ (w1) (cons w1 t)))]
         [(∘ L1 L2)          (∘ (K L1) (K L2))]

         [(→ (and e (? ε?)) f)
          (ε (for/set ([t (parse-null e)]) (f t)))]

         [(→ (I (∘ (nullp t) L2)) f)   (→ (K L2) (λ (w2) (f (cons t w2))))]
         [(→ (I (→ L f)) g)            (→ (K L) (compose g f))]
         [(→ L f)                      (→ (K L) f)]
         )))))

(define (DK c L) (K (D c L)))

(define (parse/compact w p #:compactor [compact K])
  (define (DK c p) (compact (D c p)))
  (parse-null (foldl DK p w)))
