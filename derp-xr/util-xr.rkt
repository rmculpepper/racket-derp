#lang racket/base
(require racket/match
         racket/promise
         racket/struct)
(provide (struct-out rec)
         rec-memoize
         fixed-point
         is?)

(struct rec (v) #:mutable #:transparent)

;; ----------------------------------------
;; Rec-memoize

(define (rec-memoize mkf)
  (define h (make-hasheq)) ;; rec => rec
  (lambda (x)
    (define (f x)
      (match x
        [(rec v)
         (cond [(hash-ref h x #f) => values]
               [else
                (define result (rec 'invalid))
                (hash-set! h x result)
                (begin0 result (set-rec-v! result (f v)))])]
        [_ (f-inner x)]))
    (define f-inner (mkf f))
    (f x)))

;; ----------------------------------------
;; Fixed points

(struct state (running? value) #:mutable)

(define (fixed-point #:bottom bottom #:top [top (gensym)] mkf)
  (lambda (x)
    (define h (make-hasheq)) ;; Promise => (mpair Boolean Result)
    (define (f x)
      (match x
        [(rec v)
         (define s (hash-ref! h x (lambda () (state #f bottom))))
         (cond [(or (state-running? s) (equal? top (state-value s)))
                (state-value s)]
               [else
                (set-state-running?! s #t)
                (let loop ([value (state-value s)])
                  (define value2 (f v))
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
