#lang racket/base

(require racket/match)
(require (only-in racket/function negate))

(provide primitive-types primitive-type? pointer? random-value-of-type)

; Primitive C types that this generator works for.
(define primitive-types '("int" "char"))

; Return #t if type is a primitive type, #f otherwise.
(define (primitive-type? type)
  ; make sure the result is a boolean
  (and (member type primitive-types) #t))

(module+ test
  (require rackunit)

  (check-pred primitive-type? "int")
  (check-pred primitive-type? "char")
  (check-pred (negate primitive-type?) "Node"))

; All non-primitive types are assumed to be pointers.
(define pointer? (negate primitive-type?))

(module+ test
  (check-pred pointer? "Node")
  (check-pred (negate pointer?) "int")
  (check-pred pointer? "int*"))

; Given a value of type corresponding to a primitive type in C, return a different
;  (in the sense of equal?) value of the same type. For all other types, return null.
; Right now, these values aren't guaranteed to be different because we use random.
(define (random-value-of-type type)
  (match type
    ["int" (random 5)]
    ["char" (integer->char (random 48 123))] ; some common printable chars
    [else null]))

(module+ test
  (check-pred number? (random-value-of-type "int"))
  (check-pred char? (random-value-of-type "char"))
  (check-pred null? (random-value-of-type "Node")))
