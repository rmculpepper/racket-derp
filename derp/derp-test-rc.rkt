#lang racket
(require "derp-core-rc.rkt"
         (only-in "derp-optimize-rc.rkt" nullable? DK parse/compact)
         "util.rkt")

; Examples

(define ab* (rec (delay (∪ (∘ ab* (∪ (token (is? 'a)) (token (is? 'b))))
                           (ε (set '()))))))

(define ab*2 (∪ (∘ (∪ (token (is? 'a)) (token (is? 'b)))
                   (rec (delay ab*2)))
                (ε (set '()))))


(parse '(a b b a) ab*)
(parse '(a b b a) ab*2)

(let ([L (foldl D ab* '(a b b a))])
  (nullable? L)  ;; force promises
  (pretty-print L))

;; ----

(parse/compact '(a b b a) ab*)
(parse/compact '(a b b a) ab*2)

(let ([L (foldl DK ab* '(a b b a))])
  (nullable? L)  ;; force promises
  (pretty-print L))

;; ----

#|
(define E (∪ (∘ E E) (ε (set 'ok))))
(parse-null E)
|#
