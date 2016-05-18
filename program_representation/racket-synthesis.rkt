#lang racket
(require "simulator-structures.rkt"
         "concurrent-object-lib.rkt"
         "../parser/parser.rkt"
         parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)
#|
Written by Mohdhar Noor for CSC494
        Computer Science Specialist, class of 2016
        Department of Computer Science, Software Engineering Research Group
        University of Toronto

 Mileston (beginning May 12, 2016) - accomplish the translation translation
 below:

This (C-like program) - <int test (int x, bool y ) {putIfAbsent(m, key, val);}>

To this (Racket struct) - (Method "test"
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
    ((method-root m) (translate m))
    ((method-node tp nm vlist p) (list (make-Method
                                        (tostring tp) (list (translate p)))))
    ((function-call-root f) (translate f))
    ((function-call-node nm args) (make-Instruction
                                   (lambda (e)
                                     ((find-method nm) e (tostring nm) ))
                                   #t
                                   0
                                   #f
                                   100))
    ((empty-node) null)))

#|
(find-method name)
  name: Name of the method, a string

  Returns a the function definition representing the method.
|#
(define (find-method name) 
                     (match name
                       ["put" amap-put]
                       ["remove" amap-remove]
                       ["get" amap-get]
                       ["putIfAbsent" amap-putIfAbsent]
                       ["contains" amap-contains]))