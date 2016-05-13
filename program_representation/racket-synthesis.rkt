#lang racket
(require "../program_represention/simulator-structures.rkt"
  parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)
#|
Written by Mohdhar Noor for CSC494
        Computer Science Specialist, class of 2016
        Department of Computer Science, Software Engineering Research Group
        University of Toronto

 Mileston (May 12, 2016) - accomplish this translation

This (C-like program) - <int test (int x, bool y ) {putIfAbsent(m, key, val);}>

To this (Racket program) - (Method "test"
        (list (Instruction
               (lambda (e) (amap-putIfAbsent e "m" "key" "val")))
|#

#|
(translate parsed-exp)
  parsed-exp: A parsed expression (AST)

  Returns a list a list of functions representing instructions.

  The subsets can be yielded in any order; however, no subset
  can appear twice.
|#
(define (translate parsed-exp)
  (match parsed-exp
    ((start-node u p) (translate p))
    ((program-node stmt next) (append (translate stmt) (translate next)))
    ((empty-node) null)))