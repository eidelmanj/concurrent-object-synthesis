#lang racket

(require (only-in "../program_representation/simulator-structures.rkt"
                  Method Method-id Method-args Method-instr-list
                  Run-method Run-method? Run-method-method Run-method-args Run-method-ret
                  Run-Method
                  C-Instruction C-Instruction-thread-id)
         "utils.rkt")

(provide bound error-traces)

; These aren't actual test cases (obviously).
(module+ fake-test
  (non-commutative-ops
   (Run-method (Method "put" '("char" "int") '())
               '(#\A 1)
               "test")
   (list (Method "put" '("char" "int") '())
         (Method "get" '("char") '())
         (Method "remove" '("char") '()))
   (make-hash)))

; The maximum number of non-commutative library calls to insert between each
;  pair of library calls in the mut when searching for linearizability errors.
(define bound (make-parameter 2))

; Primitive C types that this generator works for.
(define primitive-types '("int" "char"))

; Given a value of type corresponding to a primitive type in C, return a different
;  (in the sense of equal?) value of the same type. For all other types, return null.
; Right now, these values aren't guaranteed to be different because we use random.
(define (random-value-of-type type)
  (match type
    ["int" (random 20)]
    ["char" (integer->char (random 48 123))] ; some common printable chars
    [else null]))

; Return an as-yet unused variable name.
(define (fresh-var) (symbol->string (gensym)))

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
         (Method-args (Run-method-method method-call))  ; argument type
         (Run-method-args method-call)))                ; argument value
  
  ; Compute the possible non-commutative arguments of each type.
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
      (λ (args) (Run-Method method args (fresh-var) #t))
      ; Find all possible values for each argument
      (apply cartesian-product
             (map (curry hash-ref args-of-type) (Method-args method)))))
   library))

(define (trace-linearizable? trace)
  ; Simulate the execution of trace.
  ; (Figure out which instructions belong to the mut and client.)
  ; Simulate each possible interleaving of mut with the instructions of client.
  ; If the result of running the trace matches the result of one of those simulations,
  ;  return #t. Otherwise return #f.
  (define-values (mut client)
    (partition (compose null? C-Instruction-thread-id) trace))
  
  ; Generate all possible interleavings of mut (as an atomic unit) with client.
  ;  (i.e. try inserting mut before, between each pair of instructions, and after.)
  (for/list ([i (add1 (length client))])
    (let-values ([(before after) (split-at client i)])
      (append before mut after)))
  #f)

(define (error-traces library method-name pointers)
  (define mut (findf (compose (curry equal? method-name) Method-id) library))
  (define lib (remove mut library))
  
  (define (generate-error-traces-after mut breakpoint variables traces)
    (define-values (before after)
      (splitf-at-after-index mut Run-method? breakpoint))
    
    (match after
      ; Base case: no breakpoints left. Just return all accumulated traces.
      ['() traces]
      
      ; Test all possible ways to insert a library call after the current method call
      ;  (including not inserting anything).
      [`(,method-call . ,tail)
       (apply append
              (map
               (λ (ops)
                 ; Instrument mut by inserting ops right after the current
                 ; method call. Test the resulting trace for linearizability. If it
                 ; is linearizable, continue to instrument the rest of mut to try and
                 ; produce an error trace. Otherwise, add the trace to the accumulated 
                 ; list of error traces.
                 (define new-variables (map Run-method-ret ops)) ; TODO: insert Create-var
                 (define new-before (append (map )before (cons method-call ops)))
                 (define trace (append new-before tail))
                 ; test the trace
                 (if (trace-linearizable? trace)
                     (generate-error-traces-after
                      trace
                      (length new-before)
                      (append variables new-variables)
                      traces)
                     (cons trace traces)))
               ; All possible ways to pick at most bound non-commutative library calls.
               (pick-at-most-n (non-commutative-ops method-call lib pointers) (bound))))]))
  (generate-error-traces-after (Method-instr-list mut) 0 '() '()))
