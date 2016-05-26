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
 (struct-out Assume-simulation)
 (struct-out Continue)
 (struct-out Empty)
 (struct-out Meta-information)
 (struct-out Sketch-placeholder))




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
;; i - (lambda (e) (amap-putIfAbsent(e "m" "key" "val"  "")))
;; i - this is the actual function ie (lambda (e) (update-thing e "jfeioa" fhfeoia" "ret"))
;; id - always 0
;; is-method - always true
;; atomic - always false
;; inner-id - line number? Unique number doesn't matter
(define-struct Empty ())
(define-struct Meta-information (obj method arg1 arg2 arg3))


;; (define-struct Assume (condition))
(define-struct Assume-meta (condition))
(define-struct Assume-simulation (condition))

(define-struct Continue ())
(define-struct Branch (condition branch1 branch2))
(define-struct Meta-branch (condition branch1 branch2))

(define-struct Single-branch (condition branch))
(define-struct Loop (condition instr-list))



(define-struct Sketch-placeholder (name))


(define (Assume? a)
  (or (Assume-meta? a) (Assume-simulation? a)))

(define (Assume-condition a)
  (cond
    [(Assume-meta? a) (Assume-meta-condition a)]
    [(Assume-simulation? a) (Assume-simulation-condition a)]))
