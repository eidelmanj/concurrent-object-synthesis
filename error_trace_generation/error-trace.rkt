#lang racket/base

(require (only-in "../program_representation/simulator-structures.rkt"
                  Method-instr-list
                  Run-method
                  C-Instruction
                  Single-branch Branch Loop
                  Create-var)
         "backtracking.rkt"
         "interpret.rkt"
         "linearizable.rkt"
         "methods.rkt"
         (only-in racket/match match)
         (only-in racket/list split-at empty?)
         (only-in racket/pretty pretty-display)
         (only-in racket/function curryr))

(provide bound error-traces)

; The maximum number of non-commutative library calls to insert between each
;  pair of library calls in the mut when searching for linearizability errors.
(define bound (make-parameter 2))

(define (one-of L)
  (match L
    ['() (fail)]
    [`(,H . ,T) (-< H (one-of T))]))

(define (stop-value) (values (void) (void) (void)))
(define (stop-value? x y z) (void? x))

(require racket/generator)
(define (instrumenter method library pointers)
  (generator ()
    (define (instrument mut breakpoint variables client)
      (define-values (before after) (split-at mut breakpoint))
      (match after
        ; Fully instrumented the method
        ['() (stop-value)]

        [`(,instr . ,tail)
         (match instr
           ; Library method call: add conflicting operations
           [(Run-method _ _ method args ret)
            (define ops (conflicting-ops (bound) instr library pointers))
            (define op (one-of ops)) ; choice point
            (define new-mut (append before (list instr) op tail))
            (define new-vars (append variables
                                     (map (curryr arg-types library) op)))
            (define new-client (append client op))
            (unless (empty? op) (yield new-mut new-vars new-client))
            (instrument new-mut (+ breakpoint (length op) 1) new-vars new-client)]

           ; if without else: instrument the branch, then continue
           [(Single-branch _ _ condition branch)
            (define new-vars variables)
            (define new-client client)
            (for ([(trace vars ops)
                   (in-producer (instrumenter branch library pointers)
                                stop-value?)])
              (set! mut
                    (append before
                            `(,(Single-branch condition trace))
                            tail))
              (set! new-vars (append variables vars))
              (set! new-client (append client ops))
              (yield mut new-vars new-client))
            (instrument mut (add1 breakpoint) new-vars new-client)]

           ; if with else: instrument both branches, then continue
           ; NOTE: this is still wrong.
           [(Branch _ _ condition b1 b2)
            (define new-mut
              (append before
                      `(,(Branch condition (instrument b1 0) (instrument b2 0)))
                      tail))
            (instrument new-mut (add1 breakpoint))]

           ; loop: instrument the loop body, then continue
           [(Loop _ _ condition body)
            (define new-vars variables)
            (define new-client client)
            (for ([(trace vars ops)
                   (in-producer
                    (instrumenter body library pointers) stop-value?)])
              (set! mut
                    (append before `(,(Loop condition trace)) tail))
              (set! new-vars (append variables vars))
              (set! new-client (append client ops))
              (yield mut new-vars new-client))
            (instrument mut (add1 breakpoint) new-vars new-client)]

           ; Anything else: continue iteration through the method
           [(C-Instruction _ _) (instrument mut (add1 breakpoint) variables client)])]))

    (instrument method 0 '() '())))

(require (only-in racket/control prompt))

(define (error-traces library method-name pointers)
  (define mut (get-method method-name library))
  (define lib (remove mut library))
  (define interpreter (make-interpreter lib))
  (define g (instrumenter (Method-instr-list mut) lib pointers))
  
  (define traces '())
  
  ; why does this work?
  (prompt
   (for ([(trace vars client) (in-producer g stop-value?)])
     ;(pretty-display (map transform trace))
     ; Three possible results: the trace is linearizable, the trace is not
     ;  linearizable, or we couldn't come up with arguments to make the trace
     ;  feasible.
     (define result (linearizable? trace mut client vars pointers lib interpreter))
     (unless (equal? result #t)
       (when (equal? result #f)
         (set! traces (append traces (list trace))))
       (fail)))
   (when (has-next?) (next)))

  (for-each
   (λ (trace)
     (pretty-display (map transform trace)) (displayln ""))
   traces))
