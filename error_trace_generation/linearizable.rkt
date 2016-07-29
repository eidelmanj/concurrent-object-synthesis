#lang racket/base

(require (only-in "../program_representation/simulator-structures.rkt"
                  Method Create-var Run-method Set-var)
         (only-in racket/list empty? first split-at range)
         (only-in racket/match match)
         (only-in racket/pretty pretty-display)
         "interpret.rkt"
         (only-in "utils.rkt" filter-hash)
         "vars.rkt")

(provide linearizable?)

(define (linearizable? trace mut client variables pointers library interpret)
  ; Simulate each possible interleaving of mut with the instructions of client.
  ; Return a pair of (bool . error-trace) where bool is #t if trace has been found to be
  ;  linearizable, #f otherwise, and error-trace is list containing a log of the
  ;  execution of trace (which is itself a list).
  ; If the trace is "infeasible", bool is #f and error-trace is an empty list.
  (match mut
    [(Method id args ret-type instr-list)
     
     (define arguments (for/list ([type args])
                         (if (member type primitive-types)
                             (random-value-of-type type)
                             (first (hash-ref pointers type)))))
     
     (define mut-ret (fresh-var))
     (define instrs (list (Create-var mut-ret ret-type)
                          (Method id args ret-type trace)
                          (Run-method id arguments mut-ret)))
     (define declarations (list*
                           (Create-var "shared" "Node")
                           (Set-var "shared"
                                    '(Node
                                      (Node
                                       (Node
                                        '()
                                        2 2 0)
                                       1 1 0)
                                      0 0 0))
                           (for/list ([var variables])
                             (Create-var (car var) (cdr var)))))
     (define vars (cons mut-ret (map car variables)))

     ; Run the instrumented method and get the results.
     (define results (interpret (append declarations instrs) vars))
     (define result-trace (hash-ref results reserved-trace-keyword))
     (define compare-results (hash-remove results reserved-trace-keyword))

     (define infeasible-witnesses (filter-hash void? results))
     
     (cond
       [(empty? infeasible-witnesses)
        ; Generate all possible interleavings of mut (as an atomic unit) with client.
        ;  (i.e. try inserting mut before, between each pair of instructions, and after.)
        (ormap
         (λ (split-index)
           (let-values ([(before after) (split-at client split-index)])
             (cons
              (equal?
               (hash-remove
                (interpret
                 (append declarations
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
                reserved-trace-keyword)
               compare-results)
              (list result-trace))))
         (range (add1 (length client))))]

       ; Couldn't find a good variable assignment.
       [else (list #f)])]))
