#lang racket/base

(require (submod "../program_representation/simulator-structures.rkt" C-structs)
         (only-in racket/function curry)
         (only-in racket/list append-map cartesian-product)
         "vars.rkt"
         (only-in "utils.rkt" pick-at-most-n)
         (only-in racket/match match))

(provide get-method conflicting-ops arg-types pretty-AST number-lines)

(define (pretty-AST AST)
  (for/list ([instr AST])
    (match instr
      [(Create-var _ _ type id) `(define ,id ,type)]
      [(Set-var _ _ id assignment) `(set! ,id ,assignment)]
      [(Lock _ _ id) `(Lock ,id)]
      [(Unlock _ _ id) `(Unlock ,id)]
      [(Return _ _ val) `(return . ,(pretty-AST (list val)))]
      [(Get-argument _ _ id) `(Arg ,id)]
      [(Get-var id) id]
      [(Run-method _ _ method args ret) `(set! ,ret (,method . ,(pretty-AST args)))]
      [(Single-branch _ _ condition branch)
       (append '(when) (pretty-AST (list condition)) (pretty-AST branch))]
      [(Loop _ _ condition instrs) `(while ,condition ,(pretty-AST instrs))]
      [(Branch _ _ condition branch1 branch2)
       `(if ,condition ,(pretty-AST branch1) ,(pretty-AST branch2))]
      [_ instr])))

; Given a method name and a library, return the corresponding Method struct in library.
(define (get-method method-name library)
  (findf (compose (curry equal? method-name) Method-id) library))

(module+ test
  (require "../program_representation/simulator-structures.rkt"
           rackunit)

  (define library (list (Method "put" '("char" "int") "int" '())
                        (Method "get" '("char") "int" '())
                        (Method "remove" '("char") "int" '())))

  (check-match (get-method "put" library) (Method "put" '("char" "int") "int" '()))
  (check-match (get-method "get" library) (Method "get" '("char") "int" '()))
  (check-match (get-method "remove" library) (Method "remove" '("char") "int" '()))
  (check-true (not (get-method "non-existent" library))))

; method-call includes the name of the library method and the given arguments.
; library should NOT include the method under test.
; pointers is a map of pointer type names to variables of that type.
; Return a complete list of library method calls that do not commute with method-call.
;
; NOTE: For now, assume nothing commutes.
; #t is just a placeholder non-null value for the thread id to distinguish it from the mut.
(define (non-commutative-ops method-call library pointers)
  (define m-args
    (map cons
         (Method-args (get-method (Run-method-method method-call) library))  ; argument type
         (Run-method-args method-call)))                                     ; argument value

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

  ; For each method, return all calls to method with all possible values for each argument.
  (append-map
   (λ (method)
     (map
      (λ (args) (Run-Method (Method-id method) args null #t)) ; fill in the return vars later
      ; Find all possible values for each argument
      (apply cartesian-product
             (map (curry hash-ref args-of-type) (Method-args method)))))
   library))

(module+ test
  (pretty-AST
   (non-commutative-ops
    (Run-method "put" '(#\A 1) "test")
    library
    (make-hash))))

(define (ret-update run-method ret)
  (match run-method
    [(Run-method tid _ method args _) (Run-Method method args ret tid)]))

(define (conflicting-ops n method-call library pointers)
  (map (λ (ops)
         (map (λ (op) (ret-update op (fresh-var)))
              ops))
       (pick-at-most-n (non-commutative-ops method-call library pointers) n)))

(module+ test
  (map pretty-AST
       (conflicting-ops
        1
        (Run-method "put" '(#\A 1) "test")
        library
        (make-hash))))

(define (arg-types op library)
  (match op
    [(Run-method _ _ method args ret) (cons ret
                                            (Method-ret-type (get-method method library)))]))

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
