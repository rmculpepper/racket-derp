;; Compaction


; Nullability:
(define/fix (nullable? l)
  #:bottom #f
  (match l
    [(∅)           #f]
    [(ε _)          #t]
    [(token _)          #f]
    [(★ _)           #t]
    [(δ p)             (nullable? p)]
    [(∪ l1 l2)       (or (nullable? l1) (nullable? l2))]
    [(∘ l1 l2)       (and (nullable? l1) (nullable? l2))]
    [(→ l1 _)        (nullable? l1)]))

; Compute the size of a set:
(define (set-size s)
  (define size 0)
  (for ([_ s])
    (set! size (+ size 1)))
  size)

(define (singleton? s)
  (eqv? (set-size s) 1))

(define (set-choose s)
  (define el #f)
  (for ([el* s])
    (set! el el*))
  el)

; Checks whether a language is the empty string:
(define/fix (is-null? l)
  #:bottom #t
  (match l
    [(∅)          #f]
    [(ε _)         #t]    
    [(token _)         #f]
    [(∪ l1 l2)      (and (is-null? l1)  (is-null? l2))]
    [(∘ l1 l2)      (and (is-null? l1)  (is-null? l2))]
    [(★ l1)         (or (is-null? l1) (is-empty? l1))]
    [(→ l1 _)       (is-null? l1)]))

; Matches a language if it is *exactly* the empty string:
(define-match-expander nullp
  (syntax-rules ()
    [(_)    (app is-null? #t)]
    [(_ el) (and (app is-null? #t) (app parse-null (and (? singleton?) (app set-choose el))))]))

; Checks whether a language is the empty set:
(define/fix (is-empty? l)
  #:bottom #t
  (match l
    [(∅)         #t]
    [(ε _)        #f]    
    [(token _)        #f]
    [(★ l1)        #f]
    [(∪ l1 l2)     (and (is-empty? l1)  (is-empty? l2))]
    [(∘ l1 l2)     (or  (is-empty? l1)  (is-empty? l2))]
    [(→ l1 _)      (is-empty? l1)]))

(define-match-expander emptyp
  (syntax-rules ()
    [(_) (app is-empty? #t)]))



;;;; Optimizations for the grammar:

(define/memoize (compact [l #:eq])
  (match l
    [(∅)       (∅)]
    [(ε S)      (ε S)]
    [(emptyp)      (∅)]
    [(nullp)       (ε (parse-null l))]
    [(token _)      l]
    
    [(★ (emptyp))  (ε (set '()))]
    [(★ l)         (★ (compact l))]
    
    [(∪ (emptyp) l2)  (compact l2)]
    [(∪ l1 (emptyp))  (compact l1)]
    
    [(∘ (nullp t) l2)  (→ (compact l2) (lambda (w2) (cons t w2)))]
    [(∘ l1 (nullp t))  (→ (compact l1) (lambda (w1) (cons w1 t)))]
    
    [(∪ l1 l2)  (∪ (compact l1) (compact l2))]
    [(∘ l1 l2)  (∘ (compact l1) (compact l2))]
    
    [(→ (and e (nullp)) f) 
     ; =>
     (ε (for/set ([t (parse-null e)]) (f t)))]
    
    [(→ (∘ (nullp t) l2) f)
     ; =>
     (→ (compact l2) (lambda (w2) (f (cons t w2))))]
    
    [(→ (→ l f) g) 
     ; =>
     (→ (compact l) (compose g f))]
    
    [(→ l f)    (→ (compact l) f)]))

