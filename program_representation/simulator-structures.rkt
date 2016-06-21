#lang racket
(require racket/match)
(provide
 (struct-out Client-pre)
 (struct-out Method)
 (struct-out Thread-list)
 (struct-out Instruction)

 ; C Instructions
 (struct-out C-Instruction)
 (struct-out Repeat-meta)
 (struct-out Meta-addition)
 (struct-out CAS)
 (struct-out Create-var)
 (struct-out Set-var)
 (struct-out Lock)
 (struct-out Unlock)
 (struct-out Return)
 (struct-out Get-argument)
 (struct-out Run-method)
 (struct-out Single-branch)
 (struct-out Loop)
 (struct-out Maybe-loop)
 (struct-out Branch)
 (struct-out Context-switch)
 ; Extra constructors for instruction structs
 Run-Method
 Get-instr-id
 (struct-out Meta-branch)
 (struct-out Meta-single-branch)
 ;; (struct-out Assume)
 Assume?
 Assume-condition
 (struct-out Assume-loop)
 (struct-out Assume-meta)
 (struct-out None)
 (struct-out Assume-simulation)
 (struct-out Continue)
 (struct-out Empty)
 (struct-out Meta-information)
 (struct-out Tuple)
 (struct-out Hole)
 (struct-out Get-var)
 (struct-out Dereference)
 (struct-out Equal)
 (struct-out Not)
 (struct-out Or)
 (struct-out New-struct)
 (struct-out Set-pointer)
 (struct-out And)
 (struct-out Arguments)
 (struct-out History)
 (struct-out Operation)
 (struct-out Interval)
 (struct-out Info)
 (struct-out Mystery-const)
 (struct-out Assume-not-meta)
 (struct-out Sketch-placeholder)
 (struct-out Is-none?)
 (struct-out Thread-Op))






(define-struct History (start-env op-list))
(define-struct Operation (op interval id))
(define-struct Interval (start end))



(struct Tuple (a b) #:transparent)
(define-struct Set-pointer (id type offset val instr-id))

(struct Mystery-const ())

#| C INSTRUCTION STRUCTS |#

;; Parent struct for all the C instructions.
;; Racket doesn't let you actually specify a value for an optional field,
;;  so it has to be mutable and set using a separate constructor if you want
;;  something other than the default.
(struct C-Instruction ([thread-id #:auto #:mutable]) #:transparent #:auto-value null)

;; Specific C instruction structures.
(struct Repeat-meta C-Instruction (instr-list which-var))
(struct Meta-addition C-Instruction (instr-list which-var))
(struct CAS C-Instruction (v1 v2 new-val ret))
(struct Create-var C-Instruction (id type instr-id))
(struct Set-var C-Instruction (id assignment instr-id))
(struct Lock C-Instruction (id instr-id) #:transparent)
(struct Unlock C-Instruction (id instr-id))
(struct Return C-Instruction (val instr-id))
(struct Get-argument C-Instruction (id))
(struct Run-method C-Instruction (method args ret))
(struct Single-branch C-Instruction (condition branch))
(struct Loop C-Instruction (condition instr-list))
(struct Maybe-loop C-Instruction (condition instr-list))
(struct Branch C-Instruction (condition branch1 branch2))
(struct Context-switch C-Instruction () #:transparent)

;; Extra constructors allowing the thread-id field to be set.
;; The names are kind of confusing, but they can be used in place of the regular
;;  constructors anywhere, so it doesn't really matter.
(define-syntax-rule (define-tid-constructor constructor-id struct-id args ...)
  (define (constructor-id args ... [thread-id null])
    (define inst (struct-id args ...))
    (unless (null? thread-id) (set-C-Instruction-thread-id! inst thread-id))
    inst))
(define-tid-constructor Run-Method Run-method method args ret)


(struct New-struct (type arg-list))

(define-struct Dereference (id type offset))
(define-struct Equal (expr1 expr2))
(define-struct Not (expr))
(define-struct Or (expr1 expr2))
(define-struct And (expr1 expr2))
(define-struct Arguments (arg-list))
(define-struct Get-var(id))
(struct Is-none? (val))


(struct None ())

(define-struct Hole (method1 interruptor  method2))

;; Data structure for client
(define-struct Client-pre (instr-list))

;; Data structure for method
(define-struct Method (id args ret-type instr-list))
;; id - name of method
;; instr-list - for now just list of Instruction structures

;; Data structure for representing instructions that should be concurrent
(define-struct Thread-list (instr-list))

;; Data structure containing an instruction from a program - also shows whether it is part of the client or the method sketch
(define-struct Instruction (i is-method id atomic inner-id rw? meta))



(struct Info (thread-id instr-id) #:transparent)


;; i - (lambda (e) (amap-putIfAbsent(e "m" "key" "val"  "")))
;; i - this is the actual function ie (lambda (e) (update-thing e "jfeioa" fhfeoia" "ret"))
;; id - always 0
;; is-method - always true
;; atomic - always false
;; inner-id - line number? Unique number doesn't matter
;; rw? - #t, if rw? is #f, then instruction is empty
;; meta - meta information object
(define-struct Empty ())
(define-struct Meta-information (obj method arg1 arg2 arg3))
;; obj - shared data structure name
;; method - name of the method from concurr lib
;; arg1/2/3 - the arguments of the function call

;; (define-struct Assume (condition))
(define-struct Assume-meta (condition))
(define-struct Assume-not-meta (condition))
(define-struct Assume-simulation (condition))
(define-struct Assume-loop (condition to-where))


(define-struct Continue (to-where))

;; (define-struct Branch (condition branch1 branch2))



(define-struct Meta-branch (condition branch1 branch2))
(define-struct Meta-single-branch (condition branch))



(define-struct Sketch-placeholder (name))


(define (Assume? a)
  (or (Assume-meta? a) (Assume-simulation? a)))

(define (Assume-condition a)
  (cond
    [(Assume-meta? a) (Assume-meta-condition a)]
    [(Assume-simulation? a) (Assume-simulation-condition a)]))

;; A representation of an operation in a trace.
;;  tid: a symbol identifying a particular thread.
;;  mid: a symbol identifying a particular operation within a thread.
(struct Thread-Op (tid mid) #:transparent)


(define (Get-instr-id x)
  (match x
    [(Lock thread-id id instr-id) instr-id]
    [(Create-var thread-id id type instr-id) instr-id]
    [(Unlock thread-id id instr-id) instr-id]
    [(Return thread-id val instr-id) instr-id]
    [(Set-pointer id type offset val instr-id) instr-id]
    [(Set-var thread-id id assignment instr-id) instr-id]
    [(Info  t-id instr-id) instr-id]
    [_
     (None)]))
