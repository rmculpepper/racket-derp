#lang racket/base
(require racket/mpair
         racket/match
         racket/promise)
(provide (struct-out rec)
         fixed-point)

(struct rec (p) #:transparent)

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