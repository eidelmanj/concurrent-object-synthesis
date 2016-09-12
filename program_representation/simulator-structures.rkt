#lang racket
(require racket/match
         "c-structs.rkt")
(provide
 (struct-out Client-pre)
 (struct-out Method)
 (struct-out Thread-list)
 (struct-out Instruction)


 (struct-out Trace)
 (struct-out Invalid-Trace)


 ; C Instructions
 (struct-out C-Instruction)



 (c-struct-out Meta-addition-choice)
 (c-struct-out Repeat-meta)
 (c-struct-out Meta-addition)
 (c-struct-out CAS)
 (c-struct-out Create-var)
 (c-struct-out Set-var)
 (c-struct-out Lock)
 (c-struct-out Unlock)
 (c-struct-out Return)
 (c-struct-out Get-argument)
 (c-struct-out Run-method)
 (c-struct-out Single-branch)
 (c-struct-out Trace-Type)

 (c-struct-out Loop)
 (c-struct-out Maybe-loop)

 (c-struct-out Branch)
 (c-struct-out Context-switch)

 (struct-out Log)

 ;; (struct-out Repeat-meta)
 ;; (struct-out Meta-addition)
 ;; (struct-out CAS)
 ;; (struct-out Create-var)
 ;; (struct-out Set-var)
 ;; (struct-out Lock)
 ;; (struct-out Unlock)
 ;; (struct-out Return)
 ;; (struct-out Get-argument)
 ;; (struct-out Run-method)
 ;; (struct-out Single-branch)


 ;; (struct-out Repeat-meta)
 ;; (struct-out Meta-addition)
 ;; (struct-out Meta-addition-choice)
 ;; (struct-out CAS)
 ;; (struct-out Create-var)
 ;; (struct-out Set-var)
 ;; (struct-out Lock)
 ;; (struct-out Unlock)
 ;; (struct-out Return)
 ;; (struct-out Get-argument)
 ;; (struct-out Run-method)
 ;; (struct-out Single-branch)

 

 (c-struct-out Single-branch-counter)

 (c-struct-out Added-CAS-Marker)
 (c-struct-out Goto)


 (c-struct-out Label)
 (c-struct-out Atomic-Start-Marker)
 (c-struct-out Atomic-End-Marker)

 command-equality-check
 trace-ids-equal?
 rest-of-traces
 has-trace?

 RW-operation
 read-operation?
 write-operation?
 has-write-op?
 has-read-op?
 get-read-pointer
 read-operation-pointer

 ;; (struct-out Loop)
 ;; (struct-out Maybe-loop)

 ;; (struct-out Branch)
 ;; (struct-out Context-switch)

 
 ; Extra constructors for instruction structs
 Run-Method
 Run-Method-instr-id
 Get-instr-id
 (struct-out Meta-branch)
 (struct-out Meta-single-branch)
 ;; (struct-out Assume)
 Assume?
 Assume-condition
 (c-struct-out Assume-loop)
 (c-struct-out Assume-meta)
 (struct-out None)
 (c-struct-out Assume-simulation)
 Assume-simulation-cnstr
 (struct-out Continue)
 (struct-out Empty)
 (struct-out Meta-information)
 (struct-out Tuple)
 (struct-out Hole)

 (struct-out Optimistic-Condition)
 (struct-out Get-var)
 (struct-out Dereference)
 (struct-out Equal)
 (struct-out Not-equal)
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
 (c-struct-out Set-pointer)
 (struct-out And)
 (struct-out Arguments)
 (struct-out Argument)
 (struct-out History)
 (struct-out Operation)
 (struct-out Interval)
 (struct-out Info)
 (struct-out Mystery-const)
 (c-struct-out Assume-not-meta)
 (struct-out Sketch-placeholder)
 (struct-out Is-none?)
 (struct-out Structure)
 (struct-out Field)

 (struct-out Binding-list)
 ;; (struct-out Thread-Op)

 freshvar
 new-meta-var
 new-loop-id)







(define-struct History (start-env op-list))
(define-struct Operation (op interval id))
(define-struct Interval (start end))



(struct Tuple (a b) #:transparent)


(struct Mystery-const ())


(struct Binding-list (parent id-list))


#| C INSTRUCTION STRUCTS |#

;; Specific C instruction structures.

(c-struct Set-pointer (id type offset val ) #:transparent)
(c-struct Repeat-meta (instr-list which-var))
(c-struct Meta-addition (instr-list which-var))
(c-struct CAS (v1 v2 new-val ret) #:transparent)
(c-struct Create-var (id type) #:transparent)
(c-struct Set-var (id assignment) #:transparent)
(c-struct Lock (id) #:transparent)
(c-struct Unlock (id) #:transparent)
(c-struct Return (val) #:transparent)
(c-struct Get-argument (id) #:transparent)
(c-struct Run-method (method args ret) #:transparent #:mutable)
(c-struct Single-branch (condition branch) #:transparent)

(c-struct Single-branch-counter (condition branch counter) #:transparent #:mutable)
(c-struct Goto (goto-addr unroll-count) #:transparent #:mutable)
;; (c-struct Single-branch-counter (condition branch [counter #:auto #:mutable]) #:transparent #:auto-value 0)
;; (c-struct Goto (goto-addr [unroll-count #:auto #:mutable]) #:transparent #:auto-value 0)


(c-struct Assume-meta (condition))
(c-struct Assume-not-meta (condition))
(c-struct Assume-simulation (condition seen-before) #:transparent #:mutable)
(define (Assume-simulation-cnstr condition)
  (make-Assume-simulation condition #f))


(c-struct Assume-loop (condition to-where))

;; (struct Set-pointer C-Instruction (id type offset val ))
;; (struct Repeat-meta C-Instruction (instr-list which-var))
;; (struct Meta-addition C-Instruction (instr-list which-var))
(c-struct Meta-addition-choice  (instr-list which-var))
;; (struct CAS C-Instruction (v1 v2 new-val ret))
;; (struct Create-var C-Instruction (id type))
;; (struct Set-var C-Instruction (id assignment))
;; (struct Lock C-Instruction (id) #:transparent)
;; (struct Unlock C-Instruction (id))
;; (struct Return C-Instruction (val))
;; (struct Get-argument C-Instruction (id))
;; (struct Run-method C-Instruction (method args ret))
;; (struct Single-branch C-Instruction (condition branch))


;; (struct Lock C-Instruction (id) #:transparent)
;; (struct Unlock C-Instruction (id) #:transparent)
;; (struct Return C-Instruction (val) #:transparent)
;; (struct Get-argument C-Instruction (id) #:transparent)
;; (struct Run-method C-Instruction (method args ret) #:transparent #:mutable)
;; (struct Single-branch C-Instruction (condition branch) #:transparent)
;; (struct Single-branch-counter C-Instruction (condition branch [counter #:auto #:mutable]) #:transparent #:auto-value 0)
;; >>>>>>> Successfully resolves optimistic concurrency example

;; (struct Goto C-Instruction (goto-addr [unroll-count #:auto #:mutable]) #:transparent #:auto-value 0)



(c-struct Label  (id))
(c-struct Atomic-Start-Marker  ())
(c-struct Atomic-End-Marker  ())
(c-struct Trace-Type (tp))

(define (RW-operation o)
  (or (CAS? o) (Set-pointer? o) (Set-var? o)))



(define (write-operation? o)
  (or (CAS? o) (Set-pointer? o)))

(define (read-operation? o)
  (and (Set-var? o)
       (Dereference? (Set-var-assignment o))))

(define (read-operation-pointer o)
  (Dereference-id (Set-var-assignment o)))




(define (has-write-op? l)
  (< 0 (length (filter (lambda (o) (write-operation? o)) l))))
(define (has-read-op? l)
  (< 0 (length (filter (lambda (o) (read-operation? o)) l))))


(define (get-read-pointer l)
  (let
      ([all-reads (filter (lambda (o)
            (and (Set-var? o) (Dereference? (Set-var-assignment o)))) l)])
    (cond
      [(empty? all-reads)
       `()]
      [else
       (cons (Dereference-id (Set-var-assignment (first all-reads)))
             (Dereference-type (Set-var-assignment (first all-reads))))])))


(c-struct Added-CAS-Marker ())





(c-struct Loop (condition instr-list) #:transparent)
(c-struct Maybe-loop (meta-var condition instr-list1 instr-list2 original-instr-list hole))

(c-struct Branch (condition branch1 branch2) #:transparent)
(c-struct Context-switch () #:transparent)



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
   (c-struct-out Repeat-meta)
   (c-struct-out Meta-addition)
   (c-struct-out CAS)
   (c-struct-out Create-var)
   (c-struct-out Set-var)
   (c-struct-out Lock)
   (c-struct-out Unlock)
   (c-struct-out Return)
   (c-struct-out Get-argument)
   (c-struct-out Run-method)
   (c-struct-out Single-branch)

   (c-struct-out Loop)
   (c-struct-out Maybe-loop)

   (c-struct-out Branch)
   (c-struct-out Context-switch)

   (struct-out Method)
   (struct-out Get-var)

   Run-Method))


(struct New-struct (type arg-list))

(define-struct Dereference (id type offset) #:transparent)
(define-struct Equal (expr1 expr2) #:transparent)
(define-struct Not-equal (expr1 expr2) #:transparent)
(define-struct Not (expr) #:transparent)
(define-struct Or (expr1 expr2) #:transparent)
(define-struct And (expr1 expr2) #:transparent)
(define-struct Arguments (arg-list) #:transparent)
(define-struct Argument (type id) #:transparent)
(define-struct Get-var(id) #:transparent)
(define-struct Add (expr1 expr2) #:transparent)
(define-struct Subtract (expr1 expr2) #:transparent)
(define-struct Divide (expr1 expr2) #:transparent)
(define-struct Multiply (expr1 expr2) #:transparent)
(define-struct Less-than (expr1 expr2) #:transparent)
(define-struct Less-than-equal (expr1 expr2) #:transparent)
(define-struct Greater-than (expr1 expr2) #:transparent)
(define-struct Greater-than-equal (expr1 expr2) #:transparent)
(struct Is-none? (val) #:transparent)
(define-struct Constant (value))
(struct Structure (id fields))
(struct Field (name type))
(struct Optimistic-Condition (meta-var))


(struct None () #:transparent)

(define-struct Hole (method1 interruptor  method2) #:transparent)

;; Data structure for client
(define-struct Client-pre (instr-list))

;; Data structure for method
(define-struct Method (id args ret-type instr-list) #:transparent)
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


(struct Invalid-Trace ())
(struct Trace (trace-id t))


(define-struct Sketch-placeholder (name))


(define (Assume? a)
  (or (Assume-meta? a) (Assume-simulation? a)))

(define (Assume-condition a)
  (cond
    [(Assume-meta? a) (Assume-meta-condition a)]
    [(Assume-simulation? a) (Assume-simulation-condition a)]))


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


(define (has-trace? t-set t)
  (> (length (filter (lambda (t-prime) (equal? (Trace-trace-id t-prime) (Trace-trace-id t)))
          t-set)) 0))

(define (trace-ids-equal? t-set1 t-set2)
  ;; (displayln "Checking trace equality")
  ;; (display "answer: ") (displayln   (equal? (length (filter (lambda (t) (not (has-trace? t-set2 t))) t-set1)) 0))


  (equal? (length (filter (lambda (t) (not (has-trace? t-set2 t))) t-set1))
     0))

(define (rest-of-traces t-list)
  ;; (displayln "Rest-of-traces")
  (filter (lambda (t)
            (not (empty? (Trace-t t))))
          
          (map (lambda (t)
                 ;; (displayln t)
                 (Trace (Trace-trace-id t) (rest (Trace-t t))))
               t-list)))

(define (command-equality-check elem1 elem2)
  ;; TODO: This is supposed to go by instr-id, but we don't have that yet
  (equal? (object-name elem1) (object-name elem2)))








;; We keep a global counter of all the new variables in the sketch that we have created so far
;; (freshvar) gives a new program variable id
(define counter (void))
(set! counter 0)
(define (freshvar)
  (set! counter (+ counter 1))
  counter)


(define meta-var-count (void))
(set! meta-var-count 0)

(define (new-meta-var)
  (set! meta-var-count (+ meta-var-count 1))
  meta-var-count)
  


(define loop-id-count (void))
(set! loop-id-count 0)

(define (new-loop-id)
  (set! loop-id-count (+ loop-id-count 1))
  loop-id-count)
