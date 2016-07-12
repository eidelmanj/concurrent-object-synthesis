#lang racket/base

(require racket/match)

(provide primitive-types random-value-of-type fresh-var)

; Primitive C types that this generator works for.
(define primitive-types '("int" "char"))

; Given a value of type corresponding to a primitive type in C, return a different
;  (in the sense of equal?) value of the same type. For all other types, return null.
; Right now, these values aren't guaranteed to be different because we use random.
(define (random-value-of-type type)
  (match type
    ["int" (random 5)]
    ["char" (integer->char (random 48 123))] ; some common printable chars
    [else null]))

; Return an as-yet unused variable name.
(define (fresh-var) (gensym))

(module+ test
  (require rackunit)

  (check-pred number? (random-value-of-type "int"))
  (check-pred char? (random-value-of-type "char"))
  (check-pred null? (random-value-of-type "Node")))