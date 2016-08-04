#lang racket/base

(require (only-in "../program_representation/simulator-structures.rkt"
                  Method Create-var Set-var
                  Run-method Run-method-args set-Run-method-args!)
         (only-in racket/function curryr)
         (only-in racket/list empty? first split-at range filter-map)
         (only-in racket/match match match-lambda)
         "interpret.rkt"
         (only-in "utils.rkt" filter-hash)
         "vars.rkt")

(provide linearizable (struct-out lin-result))

(struct lin-result (result trace client))

; args is a list of instruction structures representing arguments. pe-args is a
;  list of the same arguments, partially evaluated.
; Merge the two lists by keeping the values in pe-args that are of primitive
;  types, and using their unevaluated versions for the rest.
; NOTE: this is all super gross.
(define (merge args pe-args)
  (map
   (λ (arg1 arg2)
     (if (or (number? arg2) (char? arg2))
         arg2
         arg1))
   args pe-args))

(define (linearizable trace mut client variables pointers library interpret arguments init)
  ; Simulate each possible interleaving of mut with the instructions of client.
  ; Return a vector of (bool error-trace client) where bool is #t if trace has been
  ;  found to be linearizable, #f otherwise, error-trace is a log of the execution of
  ;  trace, and client is the list of client instructions inserted into mut.
  ; If the trace is "infeasible", bool is #f and error-trace and client are both null.
  (match mut
    [(Method id args ret-type instr-list)

     (define mut-ret (fresh-var))
     (define instrs (list (Create-var mut-ret ret-type)
                          (Method id args ret-type trace)
                          (Run-method id arguments mut-ret)))
     (define declarations (for/list ([var variables])
                            (Create-var (car var) (cdr var))))
     (define vars (cons mut-ret (map car variables)))

     ; Run the instrumented method and get the results.
     (define results (interpret (append init declarations instrs) vars))
     (define result-trace (hash-ref results reserved-trace-keyword))
     (define compare-results (hash-remove results reserved-trace-keyword))

     ; Update the client to use partially-evaluated arguments.
     ; We need this because some of the instrumented calls use arguments
     ;  depending on variables local to the MUT.
     ; NOTE: This is super gross.
     (define pe-client
       (filter-map (match-lambda
                     [`(,pe-args . ,op)
                      (set-Run-method-args! op (merge (Run-method-args op) pe-args))
                      op]
                     [_ #f])
                   result-trace))
     ; Right now, client operations in the result trace are "tagged" with
     ;  partially-evaluated versions of their arguments. Now that we've processed
     ;  those, get rid of them.
     ; NOTE: This is super gross.
     (set! result-trace
           (map (λ (op) (if (pair? op)
                            (cdr op)
                            op))
                result-trace))

     (define infeasible-witnesses (filter-hash void? results))

     (cond
       [(empty? infeasible-witnesses)
        ; Generate all possible interleavings of mut (as an atomic unit) with client.
        ;  (i.e. try inserting mut before, between each pair of instructions, and after.)
        (define linearizable?
          (ormap
           (λ (split-index)
             (let-values ([(before after) (split-at pe-client split-index)])
               (define this-result
                 (hash-remove
                  (interpret
                   (append init
                           declarations
                           `(,(Create-var mut-ret ret-type)
                             ,(Create-var 'dummy 'int)
                             ,mut
                             ,(Method 'extension2 args ret-type ; todo fix name
                                      (append
                                       before
                                       `(,(Run-method id arguments mut-ret))
                                       after))
                             ,(Run-method 'extension2 arguments 'dummy)))
                   vars)
                  reserved-trace-keyword))

               (equal? this-result compare-results)))
           (range (add1 (length pe-client)))))

        (lin-result linearizable? result-trace client)]

       ; Couldn't find a good variable assignment.
       [else (lin-result #f null null)])]))
