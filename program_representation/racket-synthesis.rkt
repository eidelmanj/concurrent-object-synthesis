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
    ((empty-node) null)
    ((start-node p) (translate p))
    ((program-node stmt next) (append (translate stmt) (translate next)))
    ((method-root m) (translate m))
    ((method-node tp nm vlist p) (list (make-Method
                                        (tostring nm) (translate p))))
    ((function-call-root f) (translate f))
    ((function-call-node nm args) (list (make-Instruction
                                   (lambda (e)
                                     (apply (find-method nm) (translate args)))
                                   #t
                                   0
                                   #f
                                   100
                                   #t
                                   null)))
    ((arg-node v next) (append (list v) (translate next)))
    ((arg-add-node v next) (append (list v) (translate next)))
    ((arg-decl id) id))
    ((decl-node tp v) (cons tp v)))

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

;(let*
;      ((test-program "int test (int x, bool y ) {int x; x = putIfAbsent(m, key, val);}")
;         (input (open-input-string test-program)))
;    (translate (simple-math-parser (lex-this simple-math-lexer input))))

;; (let ((input (open-input-string "int test (int x, bool y ) {int x = putIfAbsent(m, key, val);}")))
;;   (display (translate (simple-math-parser (lex-this simple-math-lexer input)))))