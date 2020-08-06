#lang racket/base
(require racket/set
         racket/pretty
         racket/promise
         "herp-core-xr.rkt")

; Examples

(define abl1 (rec 'invalid))
(set-rec-v! abl1 (∪ (∘ abl1 (∪ (token 'a) (token 'b)))
                    (ε)))

#;
(define abl2 (∪ (∘ (rec (delay abl2)) (∪ (token 'a) (token 'b)))
                (ε)))

(define abr1 (rec 'invalid))
(set-rec-v! abr1 (∪ (∘ (∪ (token 'a) (token 'b))
                       abr1)
                    (ε)))

#;
(define abr2 (∪ (∘ (∪ (token 'a) (token 'b))
                   (rec (delay abr2)))
                (ε)))

(recognizes? '(a b b a) abl1)
#;(recognizes? '(a b b a) abl2)

(recognizes? '(a b b a) abr1)
#;(recognizes? '(a b b a) abr2)

;;(pretty-print ab*)

(let ([L (foldl D abl1 '(a b b a))])
  (nullable? L)  ;; force promises
  (pretty-print L))
