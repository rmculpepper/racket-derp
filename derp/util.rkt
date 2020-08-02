#lang racket/base
(require (for-syntax racket/base)
         racket/match
         racket/promise
         racket/struct)

;; ----------------------------------------
;; Lazy structs

(provide define-lazy-struct)

(define-syntax (define-lazy-struct stx)
  (syntax-case stx ()
    [(_ name {field ...})
     (with-syntax ([($name) (generate-temporaries #'(name))])
       #'(begin
           (define-struct $name {field ...}
             #:transparent #:reflection-name 'name)
           (define-match-expander name 
             (syntax-rules ()
               [(_ field ...)
                ($name (app force field) ...)])
             (syntax-rules ()
               [(_ field ...)
                ($name (delay field) ...)]))))]))

;; ----------------------------------------
;; Memoization

(provide define/memoize)

(define-syntax make-weak-hash-trie
  (syntax-rules ()
    [(_ #:eq eq ...)      (make-weak-hasheq)]
    [(_ #:eqv eq ...)     (make-weak-hasheqv)]
    [(_ #:equal eq ...)   (make-weak-hash)]))

(define-syntax weak-hash-trie-get!
  (syntax-rules ()
    [(_ t [eq] [x] lazy-val)
     (let ([$t t]
           [$x x])
       (if (hash-has-key? $t $x)
           (hash-ref $t $x)
           (let ([val lazy-val])
             (hash-set! $t $x val)
             val)))]
    [(_ t [eq1 eq2 eq3 ...] [x1 x2 x3 ...] lazy-val)
     (let ([$t t])
       (if (hash-has-key? t x1)
           (let ([t2 (hash-ref t x1)])
             (weak-hash-trie-get! t2 [eq2 eq3 ...] [x2 x3 ...] lazy-val))
           (let ([t2 (make-weak-hash-trie eq2 eq3 ...)])
             (hash-set! t x1 t2)
             (weak-hash-trie-get! t2 [eq2 eq3 ...] [x2 x3 ...] lazy-val))))]))

; Define a function that is memoized by default:
(define-syntax define/memoize 
  (syntax-rules ()
    [(_ (f [v eq] ...) body ...)
     (define/memoize (f v ...) #:order ([v eq] ...) body ...)]
    [(_ (f v ...) #:order ([v* eq] ...) body ...)
     (define f (let ((cache (make-weak-hash-trie eq ...))
                     ($f    (lambda (v ...) (let ([v* v] ...) body ...))))
                 (lambda (v ...)
                   (let ([v* v] ...)
                     (weak-hash-trie-get! cache [eq ...] [v ...] ($f v ...))))))]
    [(_ (f v ...) body ...)
     (define/memoize (f [v #:equal] ...) body ...)]))

;; ----------------------------------------
;; Fixed points

(provide define/fix)

; Generic tools:
(define-syntax while
  (syntax-rules ()
    [(_ cond body ...)
     (letrec ((lp (λ () (when cond body ... (lp)))))
       (lp))]))

; Define a recursive (yet monotonic) function over a mutually recursive
; graph by computing its fixed point:
(define-syntax define/fix
  (syntax-rules ()
    [(_ (f x) #:bottom bottom body ...)
     (define f (let ((cache     (make-weak-hasheq))
                     (changed?  (make-parameter 'error-changed))
                     (running?  (make-parameter #f))
                     (visited   (make-parameter 'error-visited)))
                 (λ (x)
                   (let ((cached? (hash-has-key? cache x))
                         (cached  (hash-ref cache x (lambda () bottom)))
                         (run?    (running?)))
                     (cond
                       [(and cached? (not run?))
                        cached]
                       [(and run? (hash-has-key? (unbox (visited)) x))
                        (if cached? cached bottom)]
                       [run? 
                        (hash-set! (unbox (visited)) x #t)
                        (let ((new-val (begin body ...)))
                          (when (not (equal? new-val cached))
                            (set-box! (changed?) #t)
                            (hash-set! cache x new-val))
                          new-val)]
                       [(and (not cached?) (not run?))
                        (parameterize ([changed? (box #t)]
                                       [running? #t]
                                       [visited (box (make-weak-hasheq))])
                          (let ([v bottom])
                            (while (unbox (changed?))
                              (set-box! (changed?) #f)
                              (set-box! (visited) (make-weak-hasheq))
                              (set! v (f x)))
                            v))])))))]))

;; ----------------------------------------
;; Curried, printable equality predicate

(provide is?)

;; is? : Any -> Any -> Boolean
;; (define ((is? x) y) (equal? x y))
(struct is? (v)
  #:property prop:custom-write
  (make-constructor-style-printer (lambda (s) 'is?) (lambda (s) (list (is?-v s))))
  #:property prop:procedure
  (lambda (self x) (equal? (is?-v self) x)))
