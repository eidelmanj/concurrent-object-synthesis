#lang racket
(require parser-tools/lex
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

(provide pp simple-math-parser
         test-parse
         lex-this
         simple-math-lexer
         num-exp
         var-node
         var-add-node
         var-decl
         arg-node
         arg-decl
         arg-add-node
         user-input-node
         start-node
         program-node
         assign-obj
         single-var
         function-call-root
         function-call-node
         expr-stmt
         assign-stmt
         expr
         var-exp
         arith-exp
         if-node
         if-root
         if-stmt
         begin-parse
         access-record
         return-node
         decl-node
         method-root
         method-node
         struct-declaration-node
         struct-declaration-root
         field-node
         loop-root
         while-node
         for-node
         comparison-exp
         bin-bool-exp
         un-bool-exp
         empty-node
         return-node
         bool-const
         null-node
         tostring)
(require racket/match)





;; Record of a GET access to a shared object
(define-struct access-record (local-var shared-var method arg-list line-num type) #:transparent)


;; Make a tostring operator for objects
(define-syntax-rule (tostring a) (format "~a" a))

(define-tokens a (NUM VAR TYPE ADDRESS))
(define-empty-tokens b (~ \. \, NULL BOOL RETURN SHARED GETTERS SETTERS ELSE STRUCT LOOP WHILE FOR DO \; = + -  < > & AND OR NOT EQUAL EOF LET IN IF \( \) \{ \} ))

(define-lex-trans number
  (syntax-rules ()
    ((_ digit)
     (re-: (re-? (re-or "-" "+")) (uinteger digit)
           (re-? (re-: "." (re-? (uinteger digit))))))))


(define-lex-trans uinteger
  (syntax-rules ()
    ((_ digit) (re-+ digit))))
(define-lex-abbrevs
  (digit10 (char-range "0" "9"))
  (number10 (number digit10))
  (identifier-characters (re-or (char-range "A" "z")
                                "?" "!" ":" "$" "%" "^" "&"))
  (basic-types (re-or "void" "int" "char" "Node" "Integer" "pthread_mutex_t" "pthread_mutex_t*" "int*" "char*" "struct"))
  (identifier (re-+ identifier-characters))
  (loop (re-or "while" "for"))
  (truth-values (re-or "true" "false")))

(define simple-math-lexer
  (lexer
   ("=" (token-=))
   ("-" (token--))
   ("+" (token-+))
   (truth-values (token-BOOL))
   ("==" (token-EQUAL))
   ("<" (token-<))
   (">" (token->))
   ("&&" (token-AND))
   ("||" (token-OR))
   ("!" (token-NOT))
   ("let" (token-LET))
   ("in" (token-IN))
   (";" (token-\;))
   ("~" (token-~))
   ("," (token-\,))
   ("(" (token-\( ))
   (")" (token-\) ))
   ("{" (token-\{ ))
   ("}" (token-\} ))
   ("if" (token-IF))
   ("else" (token-ELSE))
   ("return" (token-RETURN))
   ("." (token-\.))
   ("shared" (token-SHARED))
   ("getters" (token-GETTERS))
   ("setters" (token-SETTERS))
   ("struct" (token-STRUCT))
   ("do" (token-DO))
   ("while" (token-WHILE))
   ("for" (token-FOR))
   ((re-+ basic-types) (token-TYPE lexeme))
   ((re-+ number10) (token-NUM (string->number lexeme)))
   (identifier      (token-VAR lexeme))
   ((re-: (re-? "&") basic-types) (token-ADDRESS lexeme))
   (loop (token-LOOP))
   ;; recursively calls the lexer which effectively skips whitespace
   (whitespace (simple-math-lexer input-port))
   ((eof) (token-EOF))))


(define-struct let-exp (var num exp))
(define-struct arith-exp (op e1 e2))
(define-struct num-exp (n))
(define-struct var-exp (i) #:transparent)
(define-struct assign-stmt (var exp))
(define-struct program-node (stmt next) #:transparent)
(define-struct expr (e))
(define-struct empty-node ())
(define-struct if-node (c p1 p2) )
(define-struct if-root (e))
(define-struct expr-stmt (e))
(define-struct if-stmt (c))
(define-struct method-node (tp nm vlist p) )
(define-struct method-root (m) )
(define-struct var-decl (tp id))
(define-struct var-node (v next))
(define-struct var-add-node (v next))
(define-struct function-call-root (f var))
(define-struct function-call-node (nm args))
(define-struct arg-decl (id))
(define-struct arg-node (v next))
(define-struct arg-add-node (v next))
(define-struct struct-declaration-node (tp nm fields))
(define-struct struct-declaration-root (struct))
(define-struct field-node (type name next))
(define-struct single-var (v))
(define-struct loop-root (loop))
(define-struct while-node (exp body))
(define-struct for-node (init condition incr body))
(define-struct comparison-exp (op expr1 expr2))
(define-struct bin-bool-exp (op expr1 expr2))
(define-struct un-bool-exp (op expr1))
(define-struct bool-const (const))
(define-struct return-node (v))
(define-struct null-node ())
;; Structure for object accesses ie x.get()
(define-struct object-access (var acc))
(define-struct assign-obj (v1 v2 o id-num type) #:transparent)

;; Start node
(define-struct start-node (p) #:transparent)

;; Manually provided information
(define-struct user-input-node (s g p))

(define-struct decl-node (tp v))


(define global-id-cnt (void))

(define (begin-parse input)
  (set! global-id-cnt 0)
  (simple-math-parser input))

(define (new-assign-obj v1 v2 o)
  (let ([new-obj (make-assign-obj v1 v2 o global-id-cnt 0)])
    (set! global-id-cnt (+ global-id-cnt 1))
    new-obj))

(define simple-math-parser
  (parser
   (start start)
   (end EOF)
   (error (lambda (x y z) (display (tostring y))))
   (tokens a b)
   (precs (left - +))
   (suppress) ;; Remove this line for debugging
   (grammar
    
    
    (start ((program) (make-start-node $1)))
    
    ;; User-given information about program
;;    (user-input ((SHARED \{ arg-list \} GETTERS \{ arg-list \} SETTERS \{ arg-list \})
;;                 (make-user-input-node $3 $7 $11)))
    
    
    
    ;; The program itself
    (exp ;((BOOL) (make-bool-const $1))
         ((NUM) (num-exp $1))
         ((VAR) (var-exp $1))
         ((NULL) (null-node))
         ((exp + exp) (make-arith-exp '+ $1 $3))
         ((exp - exp) (make-arith-exp '- $1 $3))
         ((exp EQUAL exp) (make-arith-exp '= $1 $3))
         ((exp < exp) (make-comparison-exp '< $1 $3))
         ((exp < = exp) (make-comparison-exp '<= $1 $4))
         ((exp > exp) (make-comparison-exp '> $1 $3))
         ((exp > = exp) (make-comparison-exp '>= $1 $4))
         ((exp AND exp) (make-bin-bool-exp '&& $1 $3))
         ((exp OR exp) (make-bin-bool-exp '|| $1 $3))
         ((NOT exp) (make-un-bool-exp '! $2))
         ((& VAR) (make-empty-node))
         ((function-call) (make-function-call-root $1 null)))
    
    (single-line-if ((IF \( exp \) \{ program \} ) (make-if-node $3 $6 (make-empty-node))))
    ;;(single-line-if ((IF \( exp \) statement) (make-if-node $3 $5 (make-empty-node))))
    (if-else ((IF \( exp \) \{ program \} ELSE \{ program \} ) (make-if-node $3 $6 $10)))
    
    (statement 
               ((VAR = function-call \;) (make-function-call-root $3 $1))
               ((VAR = exp \;) (make-assign-stmt $1 $3))
               ((method-declaration) (make-method-root $1))
               ((function-call \;) (make-function-call-root $1 null))
               ((VAR \. object-access \;) (make-object-access $1 $3))
               ;((VAR = VAR \. object-access \;) (new-assign-obj $1 $3 $5))
               ((RETURN exp \;) (make-return-node $2))
               ((single-line-if ) (make-if-stmt (make-if-root $1)))
               ((TYPE VAR \;) (make-decl-node $1 $2))
               ((struct-declaration) (make-struct-declaration-root $1))
               ((if-else) (make-if-stmt (make-if-root $1)))
               ((loop) (make-loop-root $1)))

    (loop ((WHILE \( exp \) \{ program \}) (make-while-node $3 $6))
          ((FOR \( exp \; exp \; exp \; \) \{ program \}) (make-for-node $3 $5 $7 $11))
          ((DO \{ program \} WHILE \( exp \) \;) (make-while-node $3 $7)))

    (field-members (() make-empty-node)
                   ((TYPE VAR \; field-members) (make-field-node $1 $2 $4)))

    (object-access ((VAR) (make-single-var $1))
                   ((function-call) (function-call-root $1 null)))
    
    (method-declaration ((TYPE VAR \( var-list \) \{ program \} ) (make-method-node $1 $2 $4 $7) ))
    
    ;; Lists of variables for method/class declarations
    (add-var (() (make-empty-node))
             ((\, TYPE VAR add-var) (make-var-add-node (make-var-decl $2 $3) $4)))
    
    (var-list (() (make-empty-node))
              ((TYPE VAR add-var) (make-var-node (make-var-decl $1 $2) $3)))
    
    ;; List of arguments for function calls
    (add-arg (() (make-empty-node))
              ((\, & VAR add-arg) (make-arg-node (make-arg-decl $3) $4))
             ((\, VAR add-arg) (make-arg-add-node (make-arg-decl $2) $3)))
    
    (arg-list (() (make-empty-node))
              ((& VAR add-arg) (make-arg-node (make-arg-decl $2) $3))
              ((VAR add-arg) (make-arg-node (make-arg-decl $1) $2)))
    
    (struct-declaration ((TYPE VAR \{ field-members \} \;) 
      (make-struct-declaration-node $1 $2 $4)))

    ;; function calls
    (function-call (( VAR \( arg-list \) ) (function-call-node $1 $3)))
    
    
    ;; Main program
    (program  (() (make-empty-node))
              ((statement program) (make-program-node $1 $2))))))


;; Pretty printer
(define (pp parsed-exp)
  (match parsed-exp
    
    ((start-node p) (pp p))
    ((program-node stmt next) (string-append (pp stmt)  "\n" (pp next)))
    ((empty-node) "")
    ((expr e) (pp e))
    ((if-node e p1 p2) (string-append "if (" (pp e) ") {" (pp p1) "} else{" (pp p2) "}" ))
    ((if-root c) (pp c))
    
    ((method-node tp nm vlist p) (string-append (tostring tp) " " (tostring nm) "(" (pp vlist) "){\n" (pp p) "}\n"))
    ((method-root m) (pp m))
    
    ;; Variable lists
    ((var-add-node v next) (string-append ", " (pp v) (pp next)))
    ((var-node v next) (string-append (pp v) (pp next)))
    ((var-decl tp id) (string-append (tostring tp) " " (tostring id)))
    
    ((single-var v) (tostring v))
    
    ((return-node v) (string-append "return " (tostring v) ";\n"))
    
    ((decl-node tp v) (string-append (tostring tp) " " (tostring v) ";"))
    
    
    ;; Function calls
    ((function-call-root func var) ((if equal? var null)
                                      (string-append (pp func) ";")
                                      (string-append (tostring var) " = " (pp func) ";" )))
    
    ((function-call-node nm args) (string-append (tostring nm) "(" (pp args) ")"))
    
    ;; Argument lists for function calls
    ((arg-add-node v next) (string-append ", " (pp v) (pp next)))
    ((arg-node v next) (string-append (pp v) (pp next)))
    ((arg-decl id) (tostring id))
    
    ;; Object accesses ie x.test()
    ((object-access v acc) (string-append (tostring v) "." (pp acc) ";"))
    ((assign-obj v1 v2 o cnt type) (string-append (tostring v1) "=" (tostring v2) "." (pp o) ))
    ((access-record v1 v2 o args cnt type)
     (if (= type 0) (string-append (tostring v1) "=" (tostring v2) "." (tostring o) (tostring args) ";") (string-append (tostring v1) "=" (tostring v2) "." (tostring o) "AndMark" (tostring args))))
    
    
    ((expr-stmt e) (string-append (pp e) ";"))
    ((if-stmt c) (pp c))
    
    ((arith-exp op e1 e2)
     (print (string-append (pp e1) (print op) (pp e2))))
    ((num-exp n) (tostring n))
    ((assign-stmt v e) (string-append (tostring v) "=" (tostring (pp e))))
    ((var-exp i) (error 'pp "undefined identifier ~a" i))))

(define (lex-this lexer input) (lambda () (lexer input)))

(define-syntax-rule (test-parse str)
  (let ((input (open-input-string str)))
    (begin-parse (lex-this simple-math-lexer input))))

;; ;; Unit Tests: 

;; ;; Should output: 
;; ;; x=5;
;; ;; x=6;
;; (let ((input (open-input-string "x = 5; x=6; ")))
;;   (display (pp (simple-math-parser (lex-this simple-math-lexer input)))))
;; (display "____\n")

;; ;; Should output: 
;; ;; if (1) {x=5;
;; ;; }{y=7;
;; ;; }
;; (let ((input (open-input-string "if (1) { x=5; } else {y=7; } ")))
;;   (display (pp (simple-math-parser (lex-this simple-math-lexer input)))))


;; (let ((input (open-input-string "int test (int x, bool y ) { x=5; } ")))
;;   (display (pp (simple-math-parser (lex-this simple-math-lexer input)))))
