#lang racket
(require "derp-core.rkt"
         (only-in "derp-optimize.rkt" parse/compact)
         "util.rkt")

; Examples

(define ab* (∪ (∘ ab* (∪ (token (is? 'a)) (token (is? 'b))))
               (ε (set '()))))

(define ab*2 (∪ (∘ (∪ (token (is? 'a)) (token (is? 'b)))
                   ab*2)
                (ε (set '()))))


(parse '(a b b a) ab*)
(pretty-print ab*)

(parse '(a b b a) ab*2)

;; ----

(parse/compact '(a b b a) ab*)
(parse/compact '(a b b a) ab*2)

;; ----

#|
(define E (∪ (∘ E E) (ε (set 'ok))))
(parse-null E)
|#
