#lang racket

#| C Struct Constructor Overloading.

   This only works in the nightly builds of Racket right now because it uses
    #:extra-name, which was added after Racket 6.5. Hopefully this will work
    properly in the next release.

   Defining a struct using c-struct instead of struct creates a struct that
    is a subtype of C-Instruction, with a constructor that accepts thread-id and
    instr-id as optional arguments (either both must be specified, or neither).

   This solves some of the problems created by using #:auto for default values.
|#

(require "simulator-structures.rkt")

(require (for-syntax racket/syntax))
(define-syntax (c-struct stx)
  (syntax-case stx ()
    [(_ id (field-id ...) struct-option ...)
     (let ([alt-id (format-id stx "alt-~a" #'id)]
           [make-id (format-id stx "make-~a" #'id)])
       (with-syntax ([alt-id alt-id]
                     [make-id make-id])
         #'(begin
             (struct id C-Instruction
               (field-id ...)
               struct-option ...
               #:name alt-id
               #:constructor-name alt-id)
             
             (define make-id
               (case-lambda
                 [(field-id ...) (alt-id null null field-id ...)]
                 [(thread-id instr-id field-id ...)
                  (alt-id thread-id instr-id field-id ...)]))

             (define-match-expander id
               (λ (pat)
                 (syntax-case pat ()
                   [(_ more (... ...)) #'(alt-id more (... ...))]))

               (syntax-id-rules ()
                 [(_ args (... ...)) (make-id args (... ...))]
                 [id alt-id])))))]))

#;(c-struct Run-method (method args ret))
