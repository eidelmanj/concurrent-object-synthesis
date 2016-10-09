#lang racket
(require "simulator-structures.rkt"
         "concurrent-object-lib.rkt"
         "../parser/parser.rkt"
         racket/port
         parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)
(provide translate
         translate-to-c)
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



(define (translate-expr expr)
  (match expr
    
    [(num-exp i) i]
    [(var-exp i)
     (match i
       [(null-node) (None)]
       [_ (Get-var (translate-expr i))])]

    [(null-node) (None)]
    [(arith-exp op expr1 expr2)
     (match op
       ['= (Equal (translate-expr expr1) (translate-expr expr2))]
       ['!= (Not (Equal (translate-expr expr1) (translate-expr expr2)))]
       [ '&& (And (translate-expr expr1) (translate-expr expr2))]
       ['||  (Or (translate-expr expr1) (translate-expr expr2))]
       [_ ;; (displayln (string-append "FOUND" (~v op)))])]
        (void)])]
     
    
    [(paren-exp e)
     (translate-expr e)]
    [(obj-deref-exp expr1 expr2)
     ;; (displayln (string-append "DEREF: " (~v expr1) (~v expr2)))
     (Dereference (translate-expr expr1) null (translate-expr expr2))]
    [_ expr]))


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
                                     (cond [(equal? (car name-args) "pthread_mutex_lock") (Lock (cdr name-args))]
                                           [(equal? (car name-args) "pthread_mutex_unlock") (Unlock (cdr name-args))]
                                           [else (Run-method (car name-args) (cdr name-args) (Get-var var))]))]
    [(function-call-node nm args) (cond
                                    [(equal? nm "pthread_mutex_lock") (cons nm (translate args))]
                                    [(equal? nm "pthread_mutex_unlock") (cons nm (translate args))]
                                    [else (cons nm (translate args))])]
    [(struct-declaration-root struct) (translate struct)]
    [(struct-declaration-node name fields) (Structure name (translate fields))]
    [(object-access var acc) (Dereference (translate var) "struct" (translate acc))]
    [(dereference-node var) (translate var)]
    [(field-node type name next) (append (list (Field name type)) (translate next))]
    [(if-stmt c) (translate c)]
    [(if-root e) (translate e)]
    [(if-node c p1 p2) (if (empty? (translate p2))
                           (Single-branch (translate-expr c) (translate p1))
                           (Branch (translate-expr c) (translate p1) (translate p2)))]
    
    [(var-node v next) (cons (translate v) (translate next))]
    [(var-add-node v next) (cons (translate v) (translate next))]
    ; TODO: change instr-id arg to a counter
    [(var-decl tp id) (Argument tp id)]
    [(arg-node v next) (Arguments (append (list (translate v)) (translate next)))]
    [(arg-add-node v next) (append (list (translate v)) (translate next))]
    [(arg-decl id) (Get-var id)]
    [(decl-node tp v) (Create-var (translate v) tp)]
    [(decl-ptr-node tp v) (Create-var (translate v) (string-replace tp "struct " ""))]
    
    [(assign-stmt var exp)



     (cond
       [(and
         (cast-exp-ptr? exp)
         (function-call-root? (cast-exp-ptr-e exp))
         (equal? (function-call-node-nm (function-call-root-f (cast-exp-ptr-e exp))) "malloc"))
        
        (Set-pointer (translate-expr var) `() `()  (New-struct `() `()))]
       [(object-deref? var)

        (Set-pointer (translate-expr (object-deref-var var)) "" (translate-expr (object-deref-deref var)) (translate-expr exp))]

       [else
             (Set-var (translate var) (translate exp))])]
    [(num-exp n)  n]
    [(var-exp i) (Get-var i)]
    [(loop-root loop) (translate loop)]
    [(while-node exp body) (Loop (translate-expr exp) (translate body))]
    [(for-node init condition incr body)
     (Loop (translate condition) (append (list init condition) (translate body)))]
    [(comparison-exp op expr1 expr2)
     (bin-op-struct op (translate expr1) (translate expr2))]
    [(bin-bool-exp op expr1 expr2)
     ;; (displayln op) (displayln (translate expr1)) (displayln (translate expr2)) (exit) 
     (bin-op-struct op (translate expr1) (translate expr2))]
    [(arith-exp op expr1 expr2)
     (bin-op-struct op (translate expr1) (translate expr2))]
    [(return-node v) (Return (translate v))]
    [(bool-const const) (Constant const)]
    [v (if (string? v)
           v
           null)]
    ))

(define operators (list (cons '+ Add)
                        (cons '- Subtract)
                        (cons '/ Divide)
                        (cons '* Multiply)
                        (cons '= Equal)
                        (cons '< Less-than)
                        (cons '<= Less-than-equal)
                        (cons '> Greater-than)
                        (cons '>= Greater-than-equal)
                        (cons '&& And)
                        (cons '!= Not-equal)
                        (cons '|| Or)
                        (cons '! Not)))

(define (bin-op-struct op expr1 expr2)
  (let ([str (filter (lambda (x)(equal? (car x) op)) operators)])
    (if (empty? str)
        null
        ((cdr (first str)) expr1 expr2))))

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

(define (translate-to-c lst)
  (if (empty? lst)
      ""
      (let ((curr-instr (first lst)))
        (match curr-instr
          [(Arguments arg-list) (translate-to-c arg-list)]
          [(Argument type id) (if (empty? (rest lst))
                                  (string-append type " " id)
                                  (string-append type " " id ", " (translate-to-c (rest lst))))]
          [(Constant val) (tostring val)]
          [(Method id args ret-type instr-list)
           (string-append "\n" ret-type " " id "(" (translate-to-c args) ") {" "\n" (translate-to-c instr-list) "}" "\n" (translate-to-c (rest lst)))]
          [(Create-var _ _ id type)
           (string-append type " " (translate-to-c (list id)) "; " (translate-to-c (rest lst)))]
          [(Set-var _ _ id assignment)
           (string-append (translate-to-c (list id)) " = " (translate-to-c (list assignment)) "; " (translate-to-c (rest lst)))]
          [(Create-var _ _ id type)
           (string-append type "\t" id "; " "\n" (translate-to-c (rest lst)))]
          [(Lock _ _ id)
           (string-append "pthhread_mutex_lock(" (translate-to-c (list id)) ");" "\n" (translate-to-c (rest lst)))]
          [(Unlock _ _ id)
           (string-append "pthhread_mutex_unlock(" (translate-to-c (list id)) ");" "\n" (translate-to-c (rest lst)))]
          [(Run-method _ _ method args ret)
           (if (equal? (translate-to-c (list ret)) null)
               (string-append method "(" (translate-to-c (list args)) ")")
               (string-append (translate-to-c (list ret)) " = " method "(" (translate-to-c (list args)) ")"))]
          [(Get-var id) id]
          [(Structure id fields)
           (string-append  id " {" "\n" (translate-to-c fields) "\n" "}; " "\n" (translate-to-c (rest lst)))]
          [(Field name type)
           (string-append type " " name ";" "\n" (translate-to-c (rest lst)))]
          [(Single-branch _ _ condition branch)
           (string-append "if(" (translate-to-c (list condition)) ") { " (translate-to-c branch) " }")]
          [(Branch _ _ c p1 p2)
           (string-append "if(" (translate-to-c (list c)) ") {" "\n" (translate-to-c p1) "\n" "} else {" "\n" (translate-to-c p2) "\n" "}")]
          [(Loop _ _ expression body)

           (string-append "while(" (translate-to-c (list expression)) "){ " (translate-to-c body) " }")]
          [(Equal expr1 expr2)

           (string-append (translate-to-c (list expr1)) " == " (translate-to-c (list expr2)))]
          [(Not expr) (string-append "!" (translate-to-c (list expr)))]
          [(Or expr1 expr2)
           (string-append (translate-to-c (translate-to-c (list expr1)) " == " (translate-to-c (list expr2))))]
          [(And expr1 expr2)
           (string-append (translate-to-c (list expr1)) " && " (translate-to-c (list expr2)))]
          [(Subtract expr1 expr2)
           (string-append (translate-to-c (list expr1)) " - " (translate-to-c (list expr2)))]
          [(Add expr1 expr2)
           (string-append (translate-to-c (list expr1)) " + " (translate-to-c (list expr2)))]
          [(Divide expr1 expr2)
           (string-append (translate-to-c (list expr1)) " / " (translate-to-c (list expr2)))]
          [(Multiply expr1 expr2)
           (string-append (translate-to-c (list expr1)) " * " (translate-to-c (list expr2)))]
          [(Less-than expr1 expr2)
           (string-append (translate-to-c (list expr1)) " < " (translate-to-c (list expr2)))]
          [(Less-than-equal expr1 expr2)
           (string-append (translate-to-c (list expr1)) " <= " (translate-to-c (list expr2)))]
          [(Greater-than expr1 expr2)
           (string-append (translate-to-c (list expr1)) " > " (translate-to-c (list expr2)))]
          [(Greater-than expr1 expr2)
           (string-append (translate-to-c (list expr1)) " >= " (translate-to-c (list expr2)))]
          [(Return C-Instruction-instr-id Return-val exp)
           (string-append "return "(translate-to-c (list exp)) ";" "\n")]
          [(Dereference id type offset) (string-append "\n" id "." offset)]
          [v (if (string? v)
                 v
                 null)]
          ))))
;(let*
;      ((test-program "int test (int x, bool y ) {int z; z = putIfAbsent(m, key, val);}")
;         (input (open-input-string test-program)))
;    (translate (simple-math-parser (lex-this simple-math-lexer input))))

;; (let ((input (open-input-string "int test (int x, bool y ) {int z = putIfAbsent(m, key, val);}")))
;;   (display (translate (simple-math-parser (lex-this simple-math-lexer input)))))

;; testing
(define in (port->string (open-input-file "../test/translation_test.c")))
;(display in)


(define lst (let*
                ((test-program in)
                 (input (open-input-string test-program)))
              (translate (simple-math-parser (lex-this simple-math-lexer input)))))

;; (display (translate-to-c lst))
