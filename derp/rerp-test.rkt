#lang racket/base
(require racket/pretty)

(require "rerp-core.rkt")
;(require "rerp-core-alt.rkt")

; Examples

;; [ab]*
(define ab* (★ (∪ (token 'a) (token 'b))))

(recognizes? '(a b b a) ab*)
(recognizes? '(a b c d) ab*)

(let ([L (foldl D ab* '(a b b a))])
  (pretty-print L))
