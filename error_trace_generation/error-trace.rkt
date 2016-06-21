#lang racket

(require (only-in "../program_representation/simulator-structures.rkt"
                  Method Method-ret-type Method-instr-list
                  Run-method Run-method? Run-method-method Run-method-ret
                  C-Instruction-thread-id
                  Create-var
                  None
                  Thread-list)
         (only-in "../cex_generalization/c-program-representation.rkt"
                  interleaving-to-sketch)
         "methods.rkt"
         "utils.rkt"
         "vars.rkt"
         (only-in racket/hash hash-union))

(provide bound error-traces)

; The maximum number of non-commutative library calls to insert between each
;  pair of library calls in the mut when searching for linearizability errors.
(define bound (make-parameter 2))

(define in-mut (compose null? C-Instruction-thread-id))

(define (linearizable? mut variables pointers library)
  ; Simulate each possible interleaving of mut with the instructions of client.
  ; If the result of running the trace matches the result of one of those simulations,
  ;  return #t. Otherwise return #f.
  
  (match mut
    [(Method id args ret-type instr-list)
     (define-values (mut-instrs client) (partition in-mut instr-list))
     
     (define arguments (for/list ([type args])
                         (if (member type primitive-types)
                             (random-value-of-type type)
                             (first (hash-ref pointers type)))))
     
     (define mut-ret (fresh-var))
     (define trace (list (Create-var mut-ret ret-type (None))
                         (Run-method id args mut-ret)))
     (define declarations (for/list ([(id type) variables])
                            (Create-var id type (None))))
     (define old-mut (Method id args ret-type mut-instrs))
     (define vars (cons mut-ret (hash-keys variables)))
     
     (displayln instr-list)
     
     ; Run the instrumented method and get the results.
     (pretty-display
      (interleaving-to-sketch
       (Thread-list (append declarations trace))
       vars
       (cons mut library)))
     
     (displayln pointers)
     
     (error "pls stop")
     
     
     ; Generate all possible interleavings of mut (as an atomic unit) with client.
     ;  (i.e. try inserting mut before, between each pair of instructions, and after.)
     #;(for/list ([i (add1 (length client))])
         (let-values ([(before after) (split-at client i)])
           (interleaving-to-sketch
            (Thread-list (append declarations before trace after))
            vars
            (cons old-mut library))))])
  
  #f)

(define (instr-update method instr-list)
  (match method
    [(Method id args ret-type old-instrs) (Method id args ret-type instr-list)]))

(define (error-traces library method-name pointers)
  (define mut (get-method method-name library))
  (define lib (remove mut library))
  
  (define (generate-error-traces-after mut breakpoint variables traces)
    (define-values (before after)
      (splitf-at-after-index (Method-instr-list mut) Run-method? breakpoint))
    
    (match after
      ; Base case: no breakpoints left. Just return all accumulated traces.
      ['() traces]
      
      ; Test all possible ways to insert a library call after the current method call
      ;  (including not inserting anything).
      [`(,method-call . ,tail)
       (append-map
        ; Instrument mut by inserting ops right after the current method call.
        (λ (ops)
          ; Update the variable type map with the new return variables.
          (define new-vars
            (hash-union
             variables
             (make-hash (map
                         (λ (op)
                           (cons (Run-method-ret op)                             ; var name
                                 (Method-ret-type
                                  (get-method (Run-method-method op) library)))) ; var type
                         ops))))
          (define new-before (append before (cons method-call ops)))
          (define new-trace (append new-before tail))
          (define new-mut (struct-copy Method mut [instr-list new-trace]))
          
          (if (linearizable? new-mut new-vars pointers library)
              ; Trace linearizable: continue to instrument the rest of mut to try and
              ;  produce an error trace.
              (generate-error-traces-after new-mut
                                           (length new-before)
                                           new-vars
                                           traces)
              ; Trace not linearizable: add to the list of error traces and backtrack.
              (cons new-trace traces)))
        ; All possible ways to pick at most bound non-commutative library calls.
        (pick-non-commutative-ops (bound) method-call lib pointers))]))
  
  (generate-error-traces-after mut 0 (make-immutable-hash) '()))