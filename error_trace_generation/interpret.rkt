#lang racket/base

(require "../program_representation/simulator-structures.rkt"
         (only-in racket/match match match-lambda define-match-expander)
         (only-in racket/list append-map)
         (only-in racket/pretty pretty-display))

(provide make-interpreter transform reserved-trace-keyword)

(define reserved-parameters-keyword 'parameters-reserved)
(define reserved-trace-keyword 'trace-reserved)

; For backwards compatibility with the old format.
; The q is short for quote. It needs a better name though.
(define-match-expander q
  (syntax-rules ()
    [(q id) (app to-symbol id)]))

; Given an argument type, generate a hopefully unique parameter name.
(define make-arg
  (let ([counter -1])
    (λ (type)
      (set! counter (add1 counter))
      (string->symbol (format "~a~a" type counter)))))

; Transform a Method struct into a function definition, optionally instrumented.
(define (make-define-lambda method [log #f])
  (match method
    [(Method (q id) args ret-type instr-list)

     (define parameters (map make-arg args))
     ;; (display "Method to interpret: ")
     ;; (displayln (transform-and-instrument instr-list log))
     `(define (,id ,@parameters)
        
        ; Support for the return keyword
        (let/cc return
          ; A list of parameters for Get-argument
          (define ,reserved-parameters-keyword (list ,@parameters))
          ; Method body
          
          ,@(transform-and-instrument instr-list log)))]))

; Return a function that takes a list of instructions and executes them
;  with the bindings associated with the functions in library.
(define (make-interpreter library)
  ; These will be evaluated to instantiate the library method definitions
  ;  in the execution environment.
  (define library-defs
    (map make-define-lambda library))


  ;; (displayln "Methods:")
  ;; (map pretty-display library-defs)

  ; This namespace requires the larger racket library and then instantiates
  ;  the library method definitions so compiled programs can just call the
  ;  methods without doing any extra work.
  (define lib-namespace
    (parameterize ([current-namespace (make-base-empty-namespace)])
      (namespace-require 'racket)
      (namespace-require 'racket/block) ; see below for why we need this

      ; Important struct definitions. Node is temporary.
      (eval '(struct Node (next key val bits) #:mutable))
      (eval '(struct None ()))
      (eval '(struct List (first last) #:mutable))

      

      ; Dummy methods for lock and unlock.
      (eval '(define (lock x) (void)))
      (eval '(define (unlock x) (void)))

      (map eval library-defs)

      (current-namespace)))

  ; A closure that evaluates the given AST in the context of the library
  ;  method definitions and returns a map of the variables of interest to
  ;  their values, along with reserved-trace-keyword to a trace of the execution.
  ; All variables of interest MUST be declared in global scope of the AST.
  (λ (ast vars-of-interest)
    ; Constructs a statement that when passed to eval, constructs a hash
    ;  table of the names of the variables of interest to their values.
    (define var-hash
      `(make-immutable-hash (list
                             (cons (quote ,reserved-trace-keyword)
                                   (reverse ,reserved-trace-keyword))
                             ,@(map (λ (var-name)
                                      `(cons (quote ,var-name) ,var-name))
                                    vars-of-interest))))

    (define to-eval
      ; Use block instead of begin so variables will be defined in a new
      ;  scope. This is necessary because otherwise calling eval would
      ;  transform the namespace.
      `(block
        (define ,reserved-trace-keyword (list))
        ,@(transform-and-instrument ast)
        ,var-hash))
    (call-with-exception-handler
     (λ (exn)
       (pretty-display to-eval)
       (raise exn))
     (λ () (eval to-eval lib-namespace)))))

; For now, assume name is either a string or a symbol.
; This is just for backwards compatibility with the old format.
(define (to-symbol name)
  (if (string? name)
      (string->symbol name)
      name))

; Return an accessor of the form (struct-id-field id).
(define (to-accessor id struct-id field)
  ; For now, assume all three arguments are strings.
  (list (string->symbol (format "~a-~a" struct-id field))
        (to-symbol id)))

; Return a mutator of the form (set-struct-id-field! id value).
(define (to-mutator id struct-id field value)
  ; For now, assume the first three arguments are strings.
  (list (string->symbol (format "set-~a-~a!" struct-id field))
        (to-symbol id)
        value))

; For recursively transforming subexpressions.
; I had no idea what to call it, so for now it's just x.
(define-match-expander x
  (syntax-rules ()
    [(x expr) (app transform expr)]))

; Transform a list of C instruction structs into a quoted S-expression to be passed
;  to eval. The log flag controls whether or not the resulting code will be instrumented
;  to provide a trace of its execution.
; Code will only be instrumented if it is in the body of a top-level Method struct.
;  All other code is left alone.
(define (transform-and-instrument instrs [log #t])
  (map transform
       (if log
           (instrument instrs)
           instrs)))

; Assuming that all the methods defined in instrs are the ones that should be logged,
;  add logging statements at appropriate points in each defined method.
(define (instrument instrs)
  (map
   (match-lambda
     [(Method id args ret body) (Method id args ret (add-log-statements body))]
     [instr instr])
   instrs))

; Add log statements for important instructions in instrs.
(define (add-log-statements instrs)
  (append-map
   (λ (instr)
     (cond
       ; Simple instruction types to be logged
       [(or (Create-var? instr)
            (Set-var? instr)
            (Set-pointer? instr)
            (Return? instr)
            (Lock? instr)
            (Unlock? instr)
            (CAS? instr)
            (Run-method? instr))
        `(,(Log instr) ,instr)]

       ; Compound instructions
       [else
        (match instr
          [(Single-branch _ _ condition branch)
           `(,(Single-branch condition (cons (Log (Assume-simulation condition #f))
                                             (add-log-statements branch))))]

          [(Branch _ _ condition b1 b2)
           `(,(Branch condition
                      (cons (Log (Assume-simulation condition #f))
                            (add-log-statements b1))
                      (cons (Log (Assume-simulation (Not condition) #f))
                            (add-log-statements b2))))]

          [(Loop _ _ condition body)
           `(,(Loop condition (cons (Log (Assume-simulation condition #f))
                                    (add-log-statements body))))]

          ; Leave everything else alone
          [else instr])]))
   instrs))

; Transform an AST into a quoted S-expression.
(define (transform instr)
  (match instr
    [(Create-var _ _ (q id) _) `(define ,id (void))]
    [(Set-var _ _ (q id) (x assignment)) `(set! ,id ,assignment)]
    [(Get-var (q id)) id]
    [(Set-pointer _ _ (x id) type offset (x val))
     ;; (display "set pointer: ")
     ;; (displayln (to-mutator id type offset val))
     


     (to-mutator id type offset val)]
    [(Return _ _ (x expr)) `(return ,expr)]

    [(New-struct (q type) parameters)
     ;; (display "new struct ") (display  type) (display ": ")
     ;; (displayln parameters)
     ;; (exit)
     `(,type ,@(map transform parameters))]

    ; Method parameters are stored in a list with id reserved-parameters-keyword.
    [(Get-argument _ _ index) `(list-ref ,reserved-parameters-keyword ,index)]

    ; This compiler is for single-thread runs only, so just ignore locks (they're
    ;  implemented with dummy methods) and don't worry about making CAS atomic.
    [(Lock _ _ id) `(lock ,id)]
    [(Unlock _ _ id) `(unlock ,id)]
    [(CAS _ _ p1 p2 new ret) (transform
                              (Set-var ret
                                       (Branch (Equal p1 p2)
                                               `(,(match p1
                                                    [(Dereference id type offset)
                                                     (Set-pointer id type offset new)]
                                                    [(Get-var id) (Set-var id new)])
                                                 #t)
                                               '(#f))))]

    ; Method calls
    [(Run-method _ _ (q id) args '()) `(,id ,@(map transform args))]
    [(Run-method _ _ (q id) args (q ret)) `(set! ,ret (,id ,@(map transform args)))]

    ; Function declarations
    [(Method _ _ _ _) (make-define-lambda instr)]

    ; Conditionals
    [(Single-branch _ _ (x condition) branch) `(when ,condition
                                                 ,@(map transform branch))]
    [(Branch _ _ (x condition) b1 b2) `(if ,condition
                                           (begin ,@(map transform b1))
                                           (begin ,@(map transform b2)))]

    ; Loops
    [(Loop _ _ (x condition) body) `(let loop ()
                                      (when ,condition
                                        (let/cc continue
                                          ,@(map transform body))
                                        (loop)))]
    [(Continue _) '(continue)]

    ; Subexpressions
    [(Equal (x expr1) (x expr2)) `(equal? ,expr1 ,expr2)]
    [(Not (x expr)) `(not ,expr)]
    [(And (x expr1) (x expr2)) `(and ,expr1 ,expr2)]
    [(Or (x expr1) (x expr2))  `(or ,expr1 ,expr2)]
    [(Is-none? (x expr)) `(null? ,expr)]
    [(None) ''()]
    [(Dereference (x id) type offset) (to-accessor id type offset)]
    [(Add (x expr1) (x expr2)) `(+ ,expr1 ,expr2)]
    [(Subtract (x expr1) (x expr2)) `(- ,expr1 ,expr2)]
    [(Less-than (x expr1) (x expr2)) `(< ,expr1 ,expr2)]
    [(Less-than-equal (x expr1) (x expr2)) `(<= ,expr1 ,expr2)]
    [(Greater-than (x expr1) (x expr2)) `(> ,expr1 ,expr2)]
    [(Greater-than-equal (x expr1) (x expr2)) `(>= ,expr1 ,expr2)]

    ; Logging
    [(Log instruction)
     `(set! ,reserved-trace-keyword
            (cons
             ,(match instruction
                [(Run-method #t null id args ret) (define pe-args (map transform args))
                                                  `(cons (list ,@pe-args) ,instruction)]
                [_ instruction])
             ,reserved-trace-keyword))]

    ; Literals
    [_ instr]))

; Returns true if id is a symbol that could be considered a
;  struct mutator, i.e. of the form set-struct-id-field!.
(define (struct-mutator? id)
  (and
   (symbol? id)
   (regexp-match? #px"^set-\\S+-\\S+!$" (symbol->string id))))

(module* logging-test #f
  (require racket/pretty)

  (define get-instrs
    `(,(Lock 1)
      ,(Create-var "cur" "Node")
      ,(Set-var "cur" (Get-argument 0))
      ,(Loop  (And (Not (Is-none? (Get-var "cur")))
                   (Not (Equal (Dereference "cur" "Node" "key") (Get-argument 1))))
              `(,(Set-var "cur" (Dereference "cur" "Node" "next"))))
      ,(Single-branch
        (Is-none? (Get-var "cur"))
        `(,(Unlock 1)
          ,(Return 0)))
      ,(Unlock 1)
      ,(Return (Dereference "cur" "Node" "val"))))
  (define get (Method "get" '("Node" "int") "int" get-instrs))
  (define i (make-interpreter '()))
  (define trace (list get
                      (Create-var 'ret 'int)
                      (Run-method 'get
                                  '((Node
                                     (Node '() 1 3 0)
                                     2 4 0)
                                    1)
                                  'ret)))
  (define result (i trace '()))
  (pretty-display (hash-ref result reserved-trace-keyword)))

(module+ test
  (require rackunit)

  ; Basic subexpressions
  (check-equal? (transform (Equal 1 2)) '(equal? 1 2))
  (check-equal? (transform (Not #t)) '(not #t))
  (check-equal? (transform (And #t #f)) '(and #t #f))
  (check-equal? (transform (Or #t #f)) '(or #t #f))
  (check-equal? (transform (Is-none? 'x)) '(null? x))
  (check-equal? (transform (None)) ''())
  (check-equal? (transform (Dereference 'test 'Node 'key)) '(Node-key test))
  (check-equal? (transform (Add 1 2)) '(+ 1 2))
  (check-equal? (transform (Subtract 1 2)) '(- 1 2))
  (check-equal? (transform (Less-than 1 2)) '(< 1 2))
  (check-equal? (transform (Less-than-equal 1 2)) '(<= 1 2))
  (check-equal? (transform (Greater-than 1 2)) '(> 1 2))
  (check-equal? (transform (Greater-than-equal 1 2)) '(>= 1 2))

  ; Simple C instructions
  (check-equal? (transform (Create-var 'x 'int)) '(define x (void)))
  (check-equal? (transform (Set-var 'x 1)) '(set! x 1))
  (check-equal? (transform (Get-var 'x)) 'x)
  (check-equal? (transform (Set-pointer 'test 'Node 'key 1)) '(set-Node-key! test 1))
  (check-equal? (transform (Lock 0)) '(lock 0))
  (check-equal? (transform (Unlock 0)) '(unlock 0))
  (check-equal? (transform (Return 'x)) '(return x))
  (check-equal? (transform (Continue 0)) '(continue))
  (check-equal? (transform (New-struct 'Node (list (None) 1 2 0))) '(Node '() 1 2 0))
  (check-equal? (transform (Get-argument 0)) `(list-ref ,reserved-parameters-keyword 0))
  (check-equal? (transform (Run-method 'remove (list (None) 1) null))
                '(remove (None) 1))
  (check-equal? (transform (Run-method 'get (list (None) 1) 'val))
                '(set! val (get '() 1)))

  ; Compound C instructions
  (check-equal? (transform (Single-branch #t (list (Add 1 2)))) '(when #t (+ 1 2)))
  (check-equal? (transform (Branch #t (list (Add 1 2)) (list (Add 3 4))))
                '(if #t (begin (+ 1 2)) (begin (+ 3 4))))
  (check-equal? (transform (Loop #t (list (Create-var 'x 'int) (Set-var 'x 1))))
                '(let loop ()
                   (when #t
                     (let/cc continue
                       (define x (void))
                       (set! x 1))
                     (loop))))
  (check-equal? (transform (CAS (Dereference 'x 'Node 'key)
                                (Dereference 'y 'Node 'key)
                                (Add 1 2)
                                'z))
                '(set! z (if (equal? (Node-key x) (Node-key y))
                             (begin
                               (set-Node-key! x (+ 1 2))
                               #t)
                             (begin
                               #f))))

  ; Some test library methods
  (define push-instrs
    `(,(Lock 1)
      ,(Create-var "cur" "Node")
      ,(Create-var "prev" "Node")
      ,(Set-var "cur" (Get-argument 0))
      ,(Set-var "prev" (Get-argument 0))
      ,(Loop (And (Not (Is-none? (Get-var "cur")))
                  (Not (Equal (Dereference "cur" "Node" "key") (Get-argument 1))))
             `(,(Set-var "prev" (Get-var "cur"))
               ,(Set-var "cur" (Dereference "cur" "Node" "next"))))
      ,(Single-branch
        (Is-none? (Get-var "cur"))
        `(,(Set-pointer "prev" "Node" "next"
                        (New-struct "Node" `(,(None)
                                             ,(Get-argument 1)
                                             ,(Get-argument 2)
                                             ,(None))))
          ,(Unlock 1)
          ,(Return (Get-argument 2))))

      ,(Set-pointer "cur" "Node" "val" (Get-argument 2))
      ,(Unlock 1)
      ,(Return (Get-argument 2))))
  (check-equal? (map transform push-instrs)
                `((lock 1)
                  (define cur (void))
                  (define prev (void))
                  (set! cur (list-ref ,reserved-parameters-keyword 0))
                  (set! prev (list-ref ,reserved-parameters-keyword 0))
                  (let loop ()
                    (when (and (not (null? cur))
                               (not (equal? (Node-key cur)
                                            (list-ref ,reserved-parameters-keyword 1))))
                      (let/cc continue
                        (set! prev cur)
                        (set! cur (Node-next cur)))
                      (loop)))
                  (when (null? cur)
                    (set-Node-next! prev (Node
                                          '()
                                          (list-ref ,reserved-parameters-keyword 1)
                                          (list-ref ,reserved-parameters-keyword 2)
                                          '()))
                    (unlock 1)
                    (return (list-ref ,reserved-parameters-keyword 2)))
                  (set-Node-val! cur (list-ref ,reserved-parameters-keyword 2))
                  (unlock 1)
                  (return (list-ref ,reserved-parameters-keyword 2))))

  (define get-instrs
    `(,(Lock 1)
      ,(Create-var "cur" "Node")
      ,(Set-var "cur" (Get-argument 0))
      ,(Loop  (And (Not (Is-none? (Get-var "cur")))
                   (Not (Equal (Dereference "cur" "Node" "key") (Get-argument 1))))
              `(,(Set-var "cur" (Dereference "cur" "Node" "next"))))
      ,(Single-branch
        (Is-none? (Get-var "cur"))
        `(,(Unlock 1)
          ,(Return 0)))
      ,(Unlock 1)
      ,(Return (Dereference "cur" "Node" "val"))))
  (check-equal? (map transform get-instrs)
                `((lock 1)
                  (define cur (void))
                  (set! cur (list-ref ,reserved-parameters-keyword 0))
                  (let loop ()
                    (when (and (not (null? cur))
                               (not (equal? (Node-key cur)
                                            (list-ref ,reserved-parameters-keyword 1))))
                      (let/cc continue
                        (set! cur (Node-next cur)))
                      (loop)))
                  (when (null? cur)
                    (unlock 1)
                    (return 0))
                  (unlock 1)
                  (return (Node-val cur))))

  (define contains-instrs
    `(,(Create-var "val" "int")
      ,(Run-method "get" (list (Get-argument 0) (Get-argument 1)) "val")
      ,(Return (Get-var "val"))))
  (check-equal? (map transform contains-instrs)
                `((define val (void))
                  (set! val (get (list-ref ,reserved-parameters-keyword 0)
                                 (list-ref ,reserved-parameters-keyword 1)))
                  (return val)))

  ; Test make-interpreter
  (define-syntax-rule (check-var-equal? <id> expected instr ...)
    (check-equal? (hash-ref (interpret `(,instr ...)
                                       '(<id>))
                            '<id>)
                  expected))

  (define test-library
    `(,(Method 'push '(Node int int) 'int push-instrs)
      ,(Method 'get '(Node int) 'int get-instrs)
      ,(Method 'contains '(Node int) 'int contains-instrs)))
  (define interpret (make-interpreter test-library))
  ; First node in the list
  (check-var-equal? ret 3
                    (Create-var 'ret 'int)
                    (Run-method 'get
                                '((Node
                                   (Node '() 1 3 0)
                                   2 4 0)
                                  1)
                                'ret))
  ; Last node in the list
  (check-var-equal? ret 4
                    (Create-var 'ret 'int)
                    (Run-method 'get
                                '((Node
                                   (Node '() 1 3 0)
                                   2 4 0)
                                  2)
                                'ret))
  ; Node not in the list (early return)
  (check-var-equal? ret 0
                    (Create-var 'ret 'int)
                    (Run-method 'get
                                '((Node
                                   (Node '() 1 3 0)
                                   2 4 0)
                                  3)
                                'ret))

  ; push
  (check-var-equal? ret 6
                    (Create-var 'ret 'int)
                    (Run-method 'push
                                '((Node
                                   (Node '() 1 3 0)
                                   2 4 0)
                                  3
                                  6)
                                'ret))

  (check-var-equal? ret 0
                    (Create-var 'ret 'int)
                    (Run-method 'contains
                                '((Node
                                   (Node '() 1 3 0)
                                   2 4 0)
                                  3)
                                'ret)))
