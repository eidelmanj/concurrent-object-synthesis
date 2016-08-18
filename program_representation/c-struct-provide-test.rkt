#lang racket

(module provide-test racket
  (require "c-structs.rkt"
           (only-in "simulator-structures.rkt"
                    set-C-Instruction-instr-id!
                    set-C-Instruction-thread-id!))
  (provide (c-struct-out Run-method))

  (c-struct Run-method (method args ret) #:transparent))


(module require-test racket
  (require (submod ".." provide-test)
           rackunit)
  (check-pred Run-method? (Run-method 'a 'b 'c))
  (check-pred Run-method? (Run-method null null 'a 'b 'c))

  (check-equal? (Run-method-method (Run-method 'a 'b 'c)) 'a)
  (check-equal? (Run-method-args (Run-method 'a 'b 'c)) 'b)
  (check-equal? (Run-method-ret (Run-method 'a 'b 'c)) 'c)

  (check-match (Run-method 'a 'b 'c) (Run-method 'a 'b 'c))
  (check-match (Run-method 'a 'b 'c) (Run-method null null 'a 'b 'c)))

(require (submod "." require-test))