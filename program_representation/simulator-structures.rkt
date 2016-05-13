#lang rosette/safe

(provide
 (struct-out Client-pre)
 (struct-out Method)
 (struct-out Thread-list)
 (struct-out Instruction)
 (struct-out Branch)
 (struct-out Loop)
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
(define-struct Instruction (i is-method id atomic inner-id))
;; i - (lambda (e) (amap-putIfAbsent(e "m" "key" "val"  "")))
;; i - this is the actual function ie (lambda (e) (update-thing e "jfeioa" fhfeoia" "ret"))
;; id - always 0
;; is-method - always true
;; atomic - always false
;; inner-id - line number? Unique number doesn't matter


(define-struct Branch (condition branch1 branch2))
(define-struct Loop (condition instr-list))

(define-struct Sketch-placeholder (name))
