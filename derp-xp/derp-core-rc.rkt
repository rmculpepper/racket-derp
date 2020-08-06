#lang racket/base
(require racket/match
         racket/set
         racket/promise
         "util-rc.rkt")
(provide (all-defined-out))

; Atomic parsers:
(struct ∅      {}          #:transparent)  ; empty set
(struct ε      {tree-set}  #:transparent)  ; empty string
(struct token  {value?}    #:transparent)  ; token class

; Compound parsers:
(struct δ {lang} #:transparent)
(struct ∪ {this that} #:transparent)
(struct ∘ {left right} #:transparent)
(struct ★ {lang} #:transparent)
(struct → {lang reduce} #:transparent)
;;(struct rec (p) #:transparent)

; Derivative:
(define (D c p)
  (define h (make-hasheq))
  (define (loop p)
    (match p
      [(∅)           (∅)]
      [(ε _)         (∅)]
      [(δ _)         (∅)]
      [(token p?)    (cond
                       [(p? c) (ε (set c))]
                       [else   (∅)])]

      [(∪ p1 p2)     (∪ (loop p1)
                        (loop p2))]
      [(★ p1)        (∘ (loop p1) p)]
      [(→ p1 f)      (→ (loop p1) f)]
      [(∘ p1 p2)     (∪ (∘ (δ p1) (loop p2))
                        (∘ (loop p1) p2))]

      [(rec pp)      (let ([pp* (hash-ref! h pp (delay (loop (force pp))))])
                       (rec pp*))]
      ))
  (loop p))

; Parsing null:
(define parse-null
  (fixed-point
   #:bottom (set)
   (lambda (parse-null)
     (lambda (p)
       (match p
         [(ε S)        S]
         [(∅)          (set)]
         [(δ p)        (parse-null p)]
         [(token _)    (set)]

         [(★ _)        (set '())]
         [(∪ p1 p2)    (set-union (parse-null p1) (parse-null p2))]
         [(∘ p1 p2)    (for*/set ([t1 (parse-null p1)]
                                  [t2 (parse-null p2)])
                         (cons t1 t2))]
         [(→ p1 f)     (for/set ([t (parse-null p1)])
                         (f t))]

         [(rec pp)     (parse-null (force pp))]
         )))))

#;
; Parsing null:
(define/fix (parse-null p)
  #:bottom (set)
  (match p
    [(ε S)        S]
    [(∅)          (set)]
    [(δ p)        (parse-null p)]
    [(token _)    (set)]

    [(★ _)        (set '())]
    [(∪ p1 p2)    (set-union (parse-null p1) (parse-null p2))]
    [(∘ p1 p2)    (for*/set ([t1 (parse-null p1)]
                             [t2 (parse-null p2)])
                    (cons t1 t2))]
    [(→ p1 f)     (for/set ([t (parse-null p1)])
                    (f t))]

    [(rec pp)     (parse-null (force pp))]
    ))

; Parse a list of tokens:
(define (parse w p)
  (parse-null (foldl D p w)))
