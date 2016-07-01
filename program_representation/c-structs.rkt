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

(require (for-syntax racket/syntax))
(define-syntax (c-struct stx)
  (syntax-case stx ()
    [(_ id (field-id ...) struct-option ...)
     (let ([make-id (format-id stx "make-~a" #'id)]
           [c-id (format-id stx "my-make-~a" #'id)])
       (with-syntax ([make-id make-id]
                     [c-id c-id])
         #'(begin
             ; Define the struct in a separate module.
             (module id racket
               (require (only-in "simulator-structures.rkt"
                                 C-Instruction))
               (provide (struct-out make-id))
               (struct id C-Instruction
                 (field-id ...)
                 struct-option ...
                 #:extra-name make-id))

             ; Import the defined struct under its extra name.
             (require 'id)

             ; Define a constructor.
             (define c-id
               (case-lambda
                 [(field-id ...) (make-id null null field-id ...)]
                 [(thread-id instr-id field-id ...)
                  (make-id thread-id instr-id field-id ...)]))

             ; Bind the id to both a transformer and a constructor.
             (define-match-expander id
               ; match expander
               (λ (pat)
                 (syntax-case pat ()
                   [(_ more (... ...)) #'(make-id more (... ...))]))
               ; constructor
               (λ (stx)
                 (syntax-case stx ()
                   [(_ arg (... ...))
                    (syntax/loc stx (c-id arg (... ...)))]))))))]))

#;(c-struct Run-method-4 (method args ret))
