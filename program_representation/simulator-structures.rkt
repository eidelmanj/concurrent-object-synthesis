#lang rosette/safe

(provide
 (struct-out Client-pre)
 (struct-out Method)
 (struct-out Thread-list)
 (struct-out Instruction)
 (struct-out Branch)
 (struct-out Meta-branch)
 (struct-out Single-branch)
 (struct-out Loop)
 ;; (struct-out Assume)
 Assume?
 Assume-condition
 (struct-out Assume-meta)
 (struct-out None)
 (struct-out Assume-simulation)
 (struct-out Continue)
 (struct-out Empty)
 (struct-out Meta-information)

 (struct-out Hole)
 (struct-out Lock)
 (struct-out Unlock)
 (struct-out Create-var)
 (struct-out Get-var)
 (struct-out Return)
 (struct-out Dereference)
 (struct-out Equal)
 (struct-out Not)
 (struct-out Or)
 (struct-out Set-var)
 (struct-out Run-method)
 (struct-out Set-pointer)
 (struct-out And)
 (struct-out Get-argument)
 (struct-out Arguments)
 (struct-out History)
 (struct-out Operation)
 (struct-out Interval)
 (struct-out Info)


 (struct-out Sketch-placeholder)
 (struct-out Thread-Op))






(define-struct History (start-env op-list))
(define-struct Operation (op interval id))
(define-struct Interval (start end))




(define-struct Set-pointer (id type offset val instr-id))

(define-struct Run-method (method args ret ))
(define-struct Lock (id instr-id))

(define-struct Create-var (id type instr-id))
(define-struct Set-var (id assignment instr-id))
(define-struct Unlock (id instr-id))
(define-struct Get-var (id))
(define-struct Return (val instr-id))
(define-struct Dereference (id type offset))
(define-struct Equal (expr1 expr2))
(define-struct Not (expr))
(define-struct Or (expr1 expr2))
(define-struct And (expr1 expr2))
(define-struct Get-argument (id))
(define-struct Arguments (arg-list))



(struct None ())

(define-struct Hole (method1 interruptor  method2))

;; Data structure for client
(define-struct Client-pre (instr-list))

;; Data structure for method
(define-struct Method (id instr-list))
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
(define-struct Assume-simulation (condition))

(define-struct Continue ())
(define-struct Branch (condition branch1 branch2))
(define-struct Meta-branch (condition branch1 branch2))

(define-struct Single-branch ( condition branch))
(define-struct Loop ( condition instr-list))



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
