#lang rosette/safe

(require (only-in racket
                  make-hash hash-ref! hash-values hash->list
                  dropf-right
                  values)
         rosette/lib/match
         (rename-in (only-in racket/match match) [match match/racket])
         (only-in "../program_representation/simulator-structures.rkt"
                  C-Instruction C-Instruction-thread-id
                  Run-method Run-method?
                  Assume-simulation?)
         (only-in "../error_trace_generation/utils.rkt"
                  filter-hash))


(provide optimal-cover (struct-out hole))

; Structure containing information about a hole in a cover.
(struct hole (before interrupt [after #:mutable]) #:transparent)




(current-bitwidth #f)

; Returns a fresh symbolic boolean variable.
(define (dynamic) (define-symbolic* b boolean?) b)

; Returns an identifier for op:
;  - op's instruction id, if op belongs to thread main-tid,
;  - op's method id, if op is a method call not belonging to thread main-tid.
(define (op->id op main-tid)
  (match/racket op
    [(C-Instruction tid iid) #:when (equal? tid main-tid) iid]
    [(Run-method _ _ mid _ _) mid]))

; Returns true if op has thread id tid, false otherwise.
; op must be an instance of struct Thread-Op.
(define (in-thread? op tid)
  (equal? (C-Instruction-thread-id op) tid))

; Returns a copy of trace truncated after the last operation by thread tid.
; PRECONDITION: trace contains at least one operation by thread tid.
(define (truncate trace tid)
  (dropf-right trace (negate (curryr in-thread? tid))))

; Given a list of lists of Thread-Ops, each representing an error trace,
; and the tid of the thread running the method under test, returns a list of
; "covers" for the interleavings. A cover is a list of hole structs. All covers
; returned have a minimal number of edges.
(define (optimal-cover interleavings main-tid)
  ; Global hash table of symbolic variables representing a hole.
  (define vars (make-hash))
  (define (negated-literal hole) (! (hash-ref! vars hole (dynamic))))

  ; Assert a disjunction of literals for each interleaving.
  (for-each
   (λ (trace)
     (assert
      (apply ||
             ; Accumulate a list of literals for the edges in this trace.
             (let loop ([last-mut -1] ; id of the last important mut operation
                        [needs-after '()] ; hole structs needing a value for after
                        [trace (truncate trace main-tid)]
                        [literals (list)])
               (match trace
                 ; Done; add any holes waiting on after instructions and return.
                 ['() (append literals (map negated-literal needs-after))]

                 ; Process the next instruction.
                 [`(,op . ,tail)
                  (define op-id (op->id op main-tid))
                  (define-values (new-last-mut new-needs-after new-literals)
                    (cond
                      ; Client operations; create a hole struct for them.
                      [(not (in-thread? op main-tid))
                       (values
                        last-mut
                        (append needs-after `(,(hole last-mut op-id null)))
                        literals)]

                      ; Important main-thread operations.
                      [(Run-method? op)
                       (values
                        op-id
                        '() ; we're going to process all these in the next line
                        ; Update all the holes waiting on this operation.
                        (append literals
                                (map (λ (h)
                                       (set-hole-after! h op-id)
                                       (negated-literal h))
                                     needs-after)))]

                      ; Unimportant main thread instructions; just ignore them.
                      [else (values last-mut needs-after literals)]))

                  (loop new-last-mut new-needs-after tail new-literals)])))))
   interleavings)

  ; Enumerate all minimal solutions.
  (let loop ([covers '()])
    ; If we start with the smallest possible solutions and work our way up,
    ; we don't have to worry about the minimality of our solutions, since ruling
    ; out an already-observed solution also rules out all supersets of it.
    (define solution (optimize #:minimize `(,(count false? (hash-values vars)))
                               #:guarantee #t))
    (cond
      [(unsat? solution) covers]
      [else (assert
             ; In each future solution, at least one of the variables set to false
             ; in this solution should take on a different value.
             ; (i.e. give me a different solution next time.)
             (apply || (filter-hash false? (model solution))))
            ; Look for the next solution.
            (define cover
              (filter-hash
               false?
               ; Unfortunately evaluate doesn't work directly on a hash table,
               ; so we have to convert to a list and back.
               (make-hash (evaluate (hash->list vars) solution))))
            (loop (cons cover covers))])))

(module+ test
  (require "../error_trace_generation/mooly-test.rkt")

  (unless (null? mooly-test-traces)
    (define opt-cover (optimal-cover mooly-test-traces null))
    (for-each pretty-print opt-cover)))
