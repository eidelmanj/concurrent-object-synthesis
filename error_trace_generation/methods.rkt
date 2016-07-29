#lang racket/base

(require (submod "../program_representation/simulator-structures.rkt" C-structs)
         (only-in racket/function curry curryr)
         (only-in racket/list append-map cartesian-product first rest)
         (only-in "interpret.rkt" transform)
         "vars.rkt"
         (only-in "utils.rkt" pick-at-most-n)
         (only-in racket/match match match-lambda))

(provide get-method conflicting-ops return-type number-lines pick-arguments)

; Given a method name and a library, return the corresponding Method struct in library.
(define (get-method method-name library)
  (findf (compose (curry equal? method-name) Method-id) library))

(module+ test
  (require "../program_representation/simulator-structures.rkt"
           rackunit)

  (define library (list (Method "put" '("char" "int") "int" '())
                        (Method "get" '("char") "int" '())))

  (check-match (get-method "put" library) (Method "put" '("char" "int") "int" '()))
  (check-match (get-method "get" library) (Method "get" '("char") "int" '()))
  (check-true (not (get-method "non-existent" library))))

; Given a Run-method struct, return a list of the argument types for the method
;  being called. The method should be defined in the given library.
(define (get-argument-types rm library)
  (Method-args (get-method (Run-method-method rm) library)))

; method-call includes the name of the library method and the given arguments.
; library should NOT include the method under test.
; pointers is a map of pointer type names to variables of that type.
; Return a complete list of library method calls that do not commute with method-call.
;  The return values in these method calls are initialized to null and should be
;  filled in.
;
; NOTE: For now, assume nothing commutes.
; #t is just a placeholder non-null value for the thread id to distinguish it from
;  the mut.
(define (non-commutative-ops method-call library pointers)
  (define m-args
    (map cons
         (get-argument-types method-call library)  ; type
         (Run-method-args method-call)))           ; value

  ; Compute the possible non-commutative arguments for each type.
  (define args-of-type
    (make-hash
     (map (λ (type)
            (cons type
                  (if (member type primitive-types)
                      (cons (random-value-of-type type)
                            (for/list ([arg m-args] ; pairs of (type . val)
                                       #:when (equal? (car arg) type))
                              (cdr arg)))
                      (hash-ref pointers type))))
          (append primitive-types (hash-keys pointers)))))

  ; For each method, return all calls to method with all possible values
  ;  for each argument.
  (append-map
   (λ (method)
     (map
      (λ (args) (Run-Method (Method-id method) args null #t))
      ; Find all possible values for each argument
      (apply cartesian-product
             (map (curry hash-ref args-of-type) (Method-args method)))))
   library))

(module+ test
  (require racket/pretty)
  (for-each
   (compose pretty-print transform)
   (non-commutative-ops
    (Run-method "put" '(#\A 1) "test") library (make-hash)))
  (displayln ""))

; Given a list of ops to follow method call method-call, appropriately name
;  their return variables.
(define (set-return-variables ops method-call)
  (define instr-id (C-Instruction-instr-id method-call))
  (define counter 1)
  (map
   (match-lambda
     [(Run-method tid _ id args _)
      (define ret-var (string->symbol (format "~a~a-~a" id instr-id counter)))
      (set! counter (add1 counter))
      (Run-Method id args ret-var tid)])
   ops))

; Given a method call method-call to a method belonging to library and a bound n,
;  return a list of all possible sequences of up to n method calls that do not
;  commute with the method. pointers is a list of pointer access instructions
;  to be used to set values for arguments involving pointers.
(define (conflicting-ops n method-call library pointers)
  (map
   ; For each list of at most n operations, update the return variable.
   (curryr set-return-variables method-call)
   (pick-at-most-n (non-commutative-ops method-call library pointers) n)))

(module+ test
  (define rm (Run-method "put" '(#\A 1) "test"))
  (set-C-Instruction-instr-id! rm 1)
  (for-each
   (λ (op-list) (pretty-print (map transform op-list)))
   (conflicting-ops 2 rm library (make-hash)))
  (displayln ""))

(define (return-type op library)
  (match op
    [(Run-method _ _ method args ret)
     (cons ret (Method-ret-type (get-method method library)))]))

(define (number-lines instrs)
  (define new-line-number!
    (let ([current-line -1])
      (λ ()
        (set! current-line (add1 current-line))
        current-line)))

  (define (number-lines-helper instrs)
    (map
     (λ (instr)
       (match instr
         [(Single-branch _ _ condition branch)
          (Single-branch condition (number-lines-helper branch))]
         [(Loop _ _ condition body) (Loop condition (number-lines-helper body))]
         [(Branch _ _ condition b1 b2) (Branch condition
                                               (number-lines-helper b1)
                                               (number-lines-helper b2))]
         [(C-Instruction _ _) (set-C-Instruction-instr-id! instr (new-line-number!))
                              instr]))
     instrs))

  (number-lines-helper instrs))

(module+ test
  (number-lines
   `(,(Lock 1)
     ,(Create-var "cur" "Node")
     ,(Create-var "prev" "Node")
     ,(Create-var 'result "int")
     ,(Set-var "cur" (Get-argument 0))
     ,(Set-var "prev" (Get-argument 0))
     ,(Set-var 'result 0)
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
         ,(Return 'result)))
     ,(Set-var 'result (Dereference 'cur 'Node 'val))
     ,(Set-pointer "cur" "Node" "val" (Get-argument 2))
     ,(Unlock 1)
     ,(Return 'result))))

; Return a list of argument lists to use for testing instrumentations of mut
;  for linearizability. mut is an instance of struct Method, library is the
;  library for the shared data structure, pointers is a
;  hash table of pointer types to lists of instances of those types, and
;  init is a list of instructions used to initialize the shared data
;  structure for testing.
;
; CURRENT ASSUMPTIONS:
; - The instructions that initialize the shared data structure for testing
;    are all and only the Run-method structs in init.
; - All arguments of primitive types given to said Run-method structs are
;    literals.
; - Each pointer in pointers is initialized in init.
; - If the mut takes n arguments of type t, at least n arguments of type
;    t occur in the arguments to the instructions that initialize the
;    shared data structure.
; - Only one instance of each pointer type exists.
(define (pick-arguments mut library pointers init)
  (define init-instrs (filter Run-method? init))
  (define init-arg-types
    (append-map (curryr get-argument-types library) init-instrs))
  (define init-arg-values
    (append-map Run-method-args init-instrs))
  (define init-args (map cons init-arg-types init-arg-values))
  ; Initialize with each type mapping to an empty list
  (define args-of-type
    (make-hash (map list (append primitive-types (hash-keys pointers)))))

  ; Add values to the hash table
  (for-each
   (λ (arg-pair)
     (hash-update!
      args-of-type
      (car arg-pair)
      (λ (v) (append v `(,(cdr arg-pair))))))
   init-args)

  ; Arguments for a value that should be in the data structure
  (for/list ([type (Method-args mut)])
    (cond
      ; "Pop" a variable of type type off the list of values of that type
      [(member type primitive-types) (define values (hash-ref args-of-type type))
                                     (hash-set! args-of-type type (rest values))
                                     (first values)]
      [else (first (hash-ref pointers type))]))
#;
  (for/list ([type (Method-args mut)])
    (if (member type primitive-types)
        (random-value-of-type type)
        (first (hash-ref pointers type)))))
