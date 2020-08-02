#lang racket
(require racket/struct
         "derp-core.rkt"
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
