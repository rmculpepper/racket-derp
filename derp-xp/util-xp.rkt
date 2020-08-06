#lang racket/base
(require racket/match
         racket/promise
         racket/struct)
(provide (struct-out rec)
         fixed-point
         is?)

(struct rec (p) #:transparent)

;; ----------------------------------------
;; Fixed points

(struct state (running? value) #:mutable)

(define (fixed-point #:bottom bottom #:top [top (gensym)] mkf)
  (lambda (x)
    (define h (make-hasheq)) ;; Promise => (mpair Boolean Result)
    (define (f x)
      (match x
        [(rec p)
         (define s (hash-ref! h p (lambda () (state #f bottom))))
         (cond [(or (state-running? s) (equal? top (state-value s)))
                (state-value s)]
               [else
                (set-state-running?! s #t)
                (define pv (force p))
                (let loop ([value (state-value s)])
                  (define value2 (f pv))
                  (unless (eqv? value2 value) (set-state-value! s value2))
                  (cond [(or (equal? value2 value)
                             (equal? value2 top))
                         (begin (set-state-running?! s #f) value2)]
                        [else (loop value2)]))])]
        [_ (f-inner x)]))
    (define f-inner (mkf f))
    (f x)))

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
