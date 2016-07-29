#lang racket
(require racket/match)
(provide
 (struct-out Client-pre)
 (struct-out Method)
 (struct-out Thread-list)
 (struct-out Instruction)

 (struct-out Trace)

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
 (struct-out Added-CAS-Marker)

 RW-operation

 (struct-out Loop)
 (struct-out Maybe-loop)

 (struct-out Branch)
 (struct-out Context-switch)

 (struct-out Log)
 ; Extra constructors for instruction structs
 Run-Method
 Run-Method-instr-id
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
 (struct-out Add)
 (struct-out Subtract)
 (struct-out Divide)
 (struct-out Multiply)
 (struct-out Less-than)
 (struct-out Less-than-equal)
 (struct-out Greater-than)
 (struct-out Greater-than-equal)
 (struct-out Constant)
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
 (struct-out Structure)
 (struct-out Field)
 (struct-out Binding-list)
 (struct-out Thread-Op))






(define-struct History (start-env op-list))
(define-struct Operation (op interval id))
(define-struct Interval (start end))



(struct Tuple (a b) #:transparent)


(struct Mystery-const ())


(struct Binding-list (parent id-list))


#| C INSTRUCTION STRUCTS |#

;; Parent struct for all the C instructions.
;; Racket doesn't let you actually specify a value for an optional field,
;;  so it has to be mutable and set using a separate constructor if you want
;;  something other than the default.
(struct C-Instruction ([thread-id #:auto #:mutable] [instr-id #:auto #:mutable]) #:transparent #:auto-value null)

;; Specific C instruction structures.
(struct Set-pointer C-Instruction (id type offset val ))
(struct Repeat-meta C-Instruction (instr-list which-var))
(struct Meta-addition C-Instruction (instr-list which-var))
(struct CAS C-Instruction (v1 v2 new-val ret))
(struct Create-var C-Instruction (id type))
(struct Set-var C-Instruction (id assignment))
(struct Lock C-Instruction (id) #:transparent)
(struct Unlock C-Instruction (id))
(struct Return C-Instruction (val))
(struct Get-argument C-Instruction (id))
(struct Run-method C-Instruction (method args ret))
(struct Single-branch C-Instruction (condition branch))

(define (RW-operation o)
  (or (CAS? o) (Set-pointer? o) (Set-var? o)))

(struct Added-CAS-Marker C-Instruction ())

(struct Assume-meta C-Instruction (condition))
(struct Assume-not-meta C-Instruction (condition))
(struct Assume-simulation C-Instruction (condition))
(struct Assume-loop C-Instruction (condition to-where))


(struct Loop C-Instruction (condition instr-list))
(struct Maybe-loop C-Instruction (meta-var condition instr-list1 instr-list2 original-instr-list hole))

(struct Branch C-Instruction (condition branch1 branch2))
(struct Context-switch C-Instruction () #:transparent)

;; Contains a trace that has an ID which we will use to fuse traces together and make sketches more
;; efficient
(struct Trace (trace-id t))

; For logging traces in the interpreter.
(struct Log (instruction) #:transparent)

;; Extra constructors allowing the thread-id field to be set.
;; The names are kind of confusing, but they can be used in place of the regular
;;  constructors anywhere, so it doesn't really matter.
(define-syntax-rule (define-tid-constructor constructor-id struct-id args ...)
  (define (constructor-id args ... [thread-id null])
    (define inst (struct-id args ...))
    (unless (null? thread-id) (set-C-Instruction-thread-id! inst thread-id))
    inst))

(define-syntax-rule (define-tid-instr-id-constructor constructor-id struct-id args ...)
  (define (constructor-id args ... [thread-id null] [instr-id null])
    (define inst (struct-id args ...))
    (unless (null? thread-id) (set-C-Instruction-thread-id! inst thread-id))
    (unless (null? instr-id) (set-C-Instruction-instr-id! inst instr-id))
    inst))


(define-tid-constructor Run-Method Run-method method args ret)
(define-tid-instr-id-constructor Run-Method-instr-id Run-method method args ret)

;; Make it easy to import just the C instruction structs.
(module* C-structs #f
  (provide
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

   (struct-out Method)
   (struct-out Get-var)

   Run-Method))


(struct New-struct (type arg-list))

(define-struct Dereference (id type offset))
(define-struct Equal (expr1 expr2))
(define-struct Not (expr))
(define-struct Or (expr1 expr2))
(define-struct And (expr1 expr2))
(define-struct Arguments (arg-list))
(define-struct Get-var(id))
(define-struct Add (expr1 expr2))
(define-struct Subtract (expr1 expr2))
(define-struct Divide (expr1 expr2))
(define-struct Multiply (expr1 expr2))
(define-struct Less-than (expr1 expr2))
(define-struct Less-than-equal (expr1 expr2))
(define-struct Greater-than (expr1 expr2))
(define-struct Greater-than-equal (expr1 expr2))
(define-struct Constant (value))
(struct Is-none? (val))
(struct Structure (fields))
(struct Field (name type))


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
  (C-Instruction-instr-id x))
  ;; (match x
  ;;   [(Lock thread-id id instr-id) instr-id]
  ;;   [(Create-var thread-id id type instr-id) instr-id]
  ;;   [(Unlock thread-id id instr-id) instr-id]
  ;;   [(Return thread-id val instr-id) instr-id]
  ;;   [(Set-pointer id type offset val instr-id) instr-id]
  ;;   [(Set-var thread-id id assignment instr-id) instr-id]
  ;;   [(Info  t-id instr-id) instr-id]
  ;;   [_
  ;;    (None)]))
