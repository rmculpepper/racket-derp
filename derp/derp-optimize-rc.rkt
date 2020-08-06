#lang racket/base
(require racket/match
         racket/promise
         racket/set
         (except-in "derp-core-rc.rkt" ∅? ε?)
         "util.rkt")
(provide (all-defined-out))


;; f is monotonic
#;
(define (fixed-point #:bottom bottom #:top [top (gensym)] mkf)
  (lambda (x)
    (define h (make-hasheq)) ;; Promise => (cons Boolean Result)
    (define (f x)
      (match x
        [(rec p)
         (match (hash-ref h p (cons #f bottom))
           [(cons #f (== top)) top]
           [(cons #f value)
            (let loop ([value value])
              (hash-set! h p (cons #t value))
              (define value2 (f (force p)))
              (cond [(or (equal? value2 value)
                         (equal? value2 top))
                     ;; done
                     (hash-set! h p (cons #f value2))
                     value2]
                    [else (loop value2)]))]
           [(cons #t value) value])]
        [_ (f-inner x)]))
    (define f-inner (mkf f))
    (f x)))

(require racket/mpair)
(define (fixed-point #:bottom bottom #:top [top (gensym)] mkf)
  (lambda (x)
    (define h (make-hasheq)) ;; Promise => (mpair Boolean Result)
    (define (f x)
      (match x
        [(rec p)
         (define s (hash-ref! h p (lambda () (mcons #f bottom))))
         (cond [(or (mcar s) (equal? top (mcdr s)))
                (mcdr s)]
               [else
                (set-mcar! s #t)
                (define pv (force p))
                (let loop ([value (mcdr s)])
                  (define value2 (f pv))
                  (unless (eqv? value2 value) (set-mcdr! s value2))
                  (cond [(or (equal? value2 value)
                             (equal? value2 top))
                         (begin (set-mcar! s #f) value2)]
                        [else (loop value2)]))])]
        [_ (f-inner x)]))
    (define f-inner (mkf f))
    (f x)))

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
         )))))

#;
; Nullability:
(define/fix (nullable? L)
  #:bottom #f
  (match L
    [(∅)        #f]
    [(ε _)      #t]
    [(token _)  #f]
    [(δ L)      (nullable? L)]
    [(∪ L1 L2)  (or  (nullable? L1) (nullable? L2))]
    [(∘ L1 L2)  (and (nullable? L1) (nullable? L2))]
    [(★ _)      #t]
    [(→ L1 _)   (nullable? L1)]
    [(rec pp)   (nullable? (force pp))]
    ))

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
         [(rec pp)      (ε? (force pp))]
         )))))

#;
;; Equal to the null language (ε):
(define/fix (ε? L)
  #:bottom #t
  (match L
    [(∅)           #f]
    [(ε _)         #t]
    [(token _)     #f]
    [(δ L)         (nullable? L)]
    [(∪ L1 L2)     (and (ε? L1) (ε? L2))]
    [(∘ L1 L2)     (and (ε? L1) (ε? L2))]
    [(★ L1)        (or  (ε? L1) (∅? L1))]
    [(→ L1 _)      (ε?  L1)]
    [(rec pp)      (ε? (force pp))]
    ))

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
   #:bottom #t #| #:top #f |#
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
         [(rec pp)    (∅? (force pp))]
         )))))

#;
; Checks whether a language is the empty set:
(define/fix (∅? L)
  #:bottom #f ;; FIXME!
  (match L
    [(∅)         #t]
    [(ε _)       #f]
    [(token _)   #f]
    [(δ L)       (not (nullable? L))]
    [(★ L1)      #f]
    [(∪ L1 L2)   (and (∅? L1) (∅? L2))]
    [(∘ L1 L2)   (or  (∅? L1) (∅? L2))]
    [(→ L1 _)    (∅?  L1)]
    [(rec pp)    (∅? (force pp))]
    ))

; Optimizing compaction.
; (K L) is an equivalent, compacted version of L.
(define (K L)
  (define (unrec v) (match v [(rec pp) (force pp)] [_ v]))
  (define-match-expander I (syntax-rules () [(I p) (app unrec p)]))
  #;(define-match-expander I (syntax-rules () [(I p) p]))
  (define h (make-hasheq)) ;; (PromiseOf Parser) -> (PromiseOf Parser)
  (define (K L)
    (match L
      [(∅)         L]
      [(ε _)       L]
      [(? ∅?)      (∅)]
      [(? ε?)      (ε (parse-null L))]
      [(token _)   L]

      [(rec pp)  (cond [(hash-ref h pp #f)
                        => (lambda (pp*) (rec pp*))]
                       [else
                        (define pp* (delay (K (force pp))))
                        (hash-set! h pp pp*)
                        (hash-set! h pp* pp*) ;; K should be idempotent
                        (rec pp*)])]

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
      ))
  (K L))

(define (DK c L) (K (D c L)))

(define (parse/compact w L #:compactor [compact K])
  (if (null? w)
      (parse-null L)
      (parse/compact (cdr w) (compact (D (car w) L)))))
