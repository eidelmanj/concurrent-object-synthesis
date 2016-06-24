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
    [(empty-node) null]
    [(null-node) null]
    [(start-node p) (translate p)]
    [(program-node stmt next) (append (list (translate stmt)) (translate next))]
    [(method-root m) (translate m)]
    ; TODO: args should be a list of argument types
    [(method-node tp nm vlist p) (make-Method
                                  (tostring nm) (translate vlist) tp (translate p))]
    [(function-call-root func var) (let ([name-args (translate func)])
                                     (cond [(equal? name-args "pthread_mutex_lock") (Lock 0 0)]
                                           [(equal? name-args "pthread_mutex_unlock") (Unlock 0 0)]
                                           [else (Run-method (car name-args) (cdr name-args) (Get-var var))]))]
    [(function-call-node nm args) (cond
                                    [(equal? nm "pthread_mutex_lock") nm]
                                    [(equal? nm "pthread_mutex_unlock") nm]
                                    [else (cons nm (translate args))])]
    [(struct-declaration-root struct) (translate struct)]
    [(struct-declaration-node nm fields) (Structure nm (translate fields))]
    [(field-node type name next) (cons (Field name type) (translate next))]
    [(if-stmt c) (translate c)]
    [(if-root e) (translate e)]
    [(if-node c p1 p2) (if (empty? (translate p2))
                           (Single-branch c (translate p1))
                           (Branch c (translate p1) (translate p2)))]
    
    [(var-node v next) (cons (translate v) (translate next))]
    [(var-add-node v next) (cons (translate v) (translate next))]
    ; TODO: change instr-id arg to a counter
    [(var-decl tp id) tp]
    [(arg-node v next) (append (list v) (translate next))]
    [(arg-add-node v next) (append (list v) (translate next))]
    [(arg-decl id) id]
    [(decl-node tp v) (cons tp (list v))]
    
    [(assign-stmt var exp) (Set-var var (translate exp) 0)]
    [(num-exp n) n]
    [(var-exp i) i]
    [(loop-root loop) (translate loop)]
    [(while-node exp body) (Loop (translate exp) (translate body))]
    [(for-node init condition incr body)
     (Loop (translate condition) (append (list init condition) (translate body)))]
    [(comparison-exp op expr1 expr2) (bin-op-struct op expr1 expr2)]
    [(bin-bool-exp op expr1 expr2) (bin-op-struct op expr2 expr2)]
    ))

(define operators (list (cons '+ Add)
                        (cons '- Subtract)
                        (cons '= Equal)
                        (cons '< Less-than)
                        (cons '<= Less-than-equal)
                        (cons '> Greater-than)
                        (cons '>= Greater-than-equal)
                        (cons '&& And)
                        (cons '|| Or)
                        (cons '! Not)))

(define (bin-op-struct op expr1 expr2)
  (let ([str (filter (lambda (x)(equal? (car x) op)) operators)])
    (if (empty? str)
        null
        ((cdr str) expr1 expr2))))

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
;      ((test-program "int test (int x, bool y ) {int z; z = putIfAbsent(m, key, val);}")
;         (input (open-input-string test-program)))
;    (translate (simple-math-parser (lex-this simple-math-lexer input))))

;; (let ((input (open-input-string "int test (int x, bool y ) {int z = putIfAbsent(m, key, val);}")))
;;   (display (translate (simple-math-parser (lex-this simple-math-lexer input)))))