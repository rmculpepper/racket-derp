#lang racket/base
(require racket/set
         racket/pretty
         "herp-core.rkt")

; Examples

(define ab* (∪ (∘ ab* (∪ (token 'a) (token 'b)))
               (ε)))

(define ab*2 (∪ (∘ (∪ (token 'a) (token 'b))
                   ab*2)
                (ε)))


(recognizes? '(a b b a) ab*)
(recognizes? '(a b b a) ab*2)

;;(pretty-print ab*)

(let ([L (foldl D ab* '(a b b a))])
  (nullable? L)
  (pretty-print L))

(recognizes? '(a b c a) ab*)
(recognizes? '(a b c a) ab*2)
