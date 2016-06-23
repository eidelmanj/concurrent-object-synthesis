#lang racket/base

(require (only-in "../program_representation/simulator-structures.rkt"
                  Method-instr-list
                  Run-method
                  C-Instruction
                  Single-branch Branch Loop)
         (only-in "../cex_generalization/c-program-representation.rkt"
                  interleaving-to-sketch)
         "backtracking.rkt"
         "methods.rkt"
         (only-in racket/hash hash-union)
         (only-in racket/match match)
         (only-in racket/list split-at empty?)
         (only-in racket/pretty pretty-display))

(provide bound error-traces)

; The maximum number of non-commutative library calls to insert between each
;  pair of library calls in the mut when searching for linearizability errors.
(define bound (make-parameter 2))

; Partition an instrumented method into mut instructions and client instructions.
; Return two values: a list of mut C-Instructions and a list of client C-Instructions.
(define (partition-mut instrs)
  (for/fold ([mut '()]
             [client '()])
            ([instr instrs])
    (match instr
      [(Single-branch _ _ condition branch)
       (define-values (mut-instrs client-instrs) (partition-mut branch))
       (values (append mut `(,(Single-branch condition mut-instrs)))
               (append client client-instrs))]
      [(Branch _ _ condition branch1 branch2)
       (define-values (mut-1 client-1) (partition-mut branch1))
       (define-values (mut-2 client-2) (partition-mut branch2))
       (values (append mut `(,(Branch condition mut-1 mut-2)))
               (append client client-1 client-2))]
      [(Loop _ _ condition instr-list)
       (define-values (mut-instrs client-instrs) (partition-mut instr-list))
       (values (append mut `(,(Loop condition mut-instrs)))
               (append client client-instrs))]
      [(C-Instruction tid _) (if (null? tid)
                                 (values (append mut `(,instr)) client)
                                 (values mut (append client `(,instr))))])))

#;
(define (linearizable? mut variables pointers library)
  ; Simulate each possible interleaving of mut with the instructions of client.
  ; If the result of running the trace matches the result of one of those simulations,
  ;  return #t. Otherwise return #f.

  (match mut
    [(Method id args ret-type instr-list)
     (define-values (mut-instrs client) (partition-mut instr-list))

     (displayln mut-instrs)
     (displayln "")
     (displayln client)
     (unless (empty? client) (error "done"))

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
     #;#;#;
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

(define (one-of L)
  (match L
    ['() (fail)]
    [`(,H . ,T) (-< H (one-of T))]))

(require racket/generator)
(define (instrumenter method library pointers)
  (generator ()
    (define (instrument mut breakpoint)
      (define-values (before after) (split-at mut breakpoint))
      (match after
        ; Fully instrumented the method
        ['() (void)]

        [`(,instr . ,tail)
         (match instr
           ; Library method call: add conflicting operations
           [(Run-method _ method args ret)
            (define ops (conflicting-ops (bound) instr library pointers))
            (define op (one-of ops)) ; choice point
            (define new-mut (append before (list instr) op tail))
            (unless (empty? op) (yield new-mut))
            (instrument new-mut (+ breakpoint (length op) 1))]

           ; if without else: instrument the branch, then continue
           [(Single-branch _ _ condition branch)
            (define new-mut mut)
            (for ([trace (in-producer (instrumenter branch library pointers) (void))])
              (set! new-mut
                    (append before
                            `(,(Single-branch condition trace))
                            tail))
              (yield new-mut))
            (instrument new-mut (add1 breakpoint))]

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
            (define new-mut mut)
            (for ([trace (in-producer (instrumenter body library pointers) (void))])
              (set! new-mut
                    (append before `(,(Loop condition trace)) tail))
              (yield new-mut))
            (instrument new-mut (add1 breakpoint))]

           ; Anything else: continue iteration through the method
           [(C-Instruction _ _) (instrument mut (add1 breakpoint))])]))

    (instrument method 0)))

; TODO: actually implement this.
(define (linearizable? trace) #t)

(require (only-in racket/control prompt))

(define (error-traces library method-name pointers)
  (define mut (get-method method-name library))
  (define lib (remove mut library))
  (define g (instrumenter (Method-instr-list mut) lib pointers))

  (define traces '())

  ; why does this work?
  (prompt
   (for ([trace (in-producer g (void))])
     (set! traces (append traces (list trace)))
     #;#;
     (pretty-display (pretty-AST trace))
     (displayln "")
     (unless (linearizable? trace) (fail)))
   (when (has-next?) (next)))

  (for-each
   (λ (trace)
     (pretty-display (pretty-AST trace)) (displayln ""))
   traces))
