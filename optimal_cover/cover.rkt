#lang rosette/safe

(require (only-in racket
                  make-hash hash-ref! hash-values hash->list
                  symbol->string string->symbol
                  dropf-right
                  set set-add
                  for/list)
         rosette/lib/match
         (only-in "../program_representation/simulator-structures.rkt"
                  Thread-Op Thread-Op-tid))

(current-bitwidth #f)

; Returns a fresh symbolic boolean variable.
(define (dynamic) (define-symbolic* b boolean?) b)

; Returns a symbol representation of op in the form 'tid/mid.
(define (op->symbol op)
  (match op
    [(Thread-Op tid mid) (string->symbol
                   (format "~a/~a" (symbol->string tid) (symbol->string mid)))]))

; Returns true if op has thread id tid, false otherwise.
; op must be an instance of struct Thread-Op.
(define (in-thread? op tid)
  (equal? (Thread-Op-tid op) tid))

; Returns a copy of trace truncated after the last operation by thread tid.
; PRECONDITION: trace contains at least one operation by thread tid.
(define (truncate trace tid)
  (dropf-right trace (negate (curryr in-thread? tid))))

; Given a list of lists of Thread-Ops, each representing an error trace,
; and the tid of the thread running the method under test, returns a list of
; "covers" for the interleavings. A cover is a list of pairs of edges and
; a boolean which is #t if the edge can be kept, #f if the edge should be
; removed. All covers returned have a minimal number of edges assigned to #f.
(define (optimal-cover interleavings main-tid)
  ; Global hash table of symbolic variables representing a pair of
  ; a set of completed operations and a pending operation.
  (define vars (make-hash))
  
  ; Assert a disjunction of literals for each interleaving.
  (for-each
   (λ (trace)
     (assert
      (apply ||
             ; Accumulate a list of literals for the edges in this trace.
             (let loop ([completed (set)]
                        [trace (truncate trace main-tid)]
                        [literals (list)])
               (match trace
                 ; Remove the extra #f literals, just in case it speeds up the solver.
                 ['() (remove* '(#f) literals)]
                 [`(,op . ,tail)
                  (loop
                   (set-add completed (op->symbol op))
                   tail
                   ; Add symbolic variables only for operations of the client thread.
                   (cons (and (not (in-thread? op main-tid))
                              (! (hash-ref! vars
                                            (cons completed (op->symbol op))
                                            (dynamic))))
                         literals))])))))
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
             (apply || (for/list ([(var value) (model solution)]
                                  #:unless value)
                         var)))
            ; Look for the next solution.
            (loop (cons (evaluate (hash->list vars) solution) covers))])))

; Convenience form for testing optimal-cover.
(define-syntax-rule (opt-cover-test
                     ((tid mid) ...)
                     ...
                     main-tid)
  (optimal-cover
   (list (list (Thread-Op 'tid 'mid) ...)
         ...)
   'main-tid))

; Example of using the convenience form:
#;
(opt-cover-test
 ((t1 m1) (t1 m2) (t2 m1) (t1 m3) (t2 m2) (t1 m4))
 ((t1 m1) (t1 m2) (t2 m1) (t1 m3) (t1 m4))
 ((t1 m1) (t1 m2) (t1 m3) (t2 m1) (t2 m2) (t1 m4))
 t1)
