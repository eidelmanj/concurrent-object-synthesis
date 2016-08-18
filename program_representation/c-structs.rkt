#lang racket

#| C Struct Constructor Overloading.

   This only works in the nightly builds of Racket right now because it uses
    #:name, which was added after Racket 6.5. Hopefully this will work
    properly in the next release.

   Defining a struct using c-struct instead of struct creates a struct that
    is a subtype of C-Instruction, with a constructor that accepts thread-id and
    instr-id as optional arguments (either both must be specified, or neither).

   It also binds the struct id to a match transformer that can match instances
    of the struct with or without the optional fields specified as pattern variables.

   This solves some of the problems created by using #:auto for default values.
|#

(provide c-struct c-struct-out (struct-out C-Instruction))

(struct C-Instruction (thread-id instr-id) #:mutable #:transparent)

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
             
             ; Custom constructor
             (define make-id
               (case-lambda
                 [(field-id ...) (alt-id null null field-id ...)]
                 [(thread-id instr-id field-id ...)
                  (alt-id thread-id instr-id field-id ...)]))

             (define-match-expander id
               ; when used in match
               (λ (pat)
                 (syntax-case pat ()
                   [(_ field-id ...) #'(alt-id _ _ field-id ...)]
                   [(_ more (... ...)) #'(alt-id more (... ...))]))

               ; when used in an expression
               (syntax-id-rules ()
                 [(_ args (... ...)) (make-id args (... ...))]
                 [id alt-id])))))]))

(require racket/provide-syntax)
(define-provide-syntax (c-struct-out stx)
  (syntax-case stx ()
    [(_ struct-id) (with-syntax ([alt-id (format-id stx "alt-~a" #'struct-id)])
                     #'(struct-out alt-id))]))

(module+ test
  (require rackunit)

  (c-struct Run-method (method args ret) #:transparent)

  (check-pred Run-method? (Run-method 'a 'b 'c))
  (check-pred Run-method? (Run-method null null 'a 'b 'c))

  (check-equal? (Run-method-method (Run-method 'a 'b 'c)) 'a)
  (check-equal? (Run-method-args (Run-method 'a 'b 'c)) 'b)
  (check-equal? (Run-method-ret (Run-method 'a 'b 'c)) 'c)

  (check-match (Run-method 'a 'b 'c) (Run-method 'a 'b 'c))
  (check-match (Run-method 'a 'b 'c) (Run-method null null 'a 'b 'c)))
