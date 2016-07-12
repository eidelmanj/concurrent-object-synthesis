#lang racket/base

(require (only-in "../program_representation/simulator-structures.rkt"
                  Method Create-var Run-method Set-var)
         (only-in racket/list empty? first split-at range)
         (only-in racket/match match)
         (only-in racket/pretty pretty-display)
         "interpret.rkt"
         "vars.rkt")

(provide linearizable?)

(define (linearizable? trace mut client variables pointers library interpret)
  ; Simulate each possible interleaving of mut with the instructions of client.
  ; If the result of running the trace matches the result of one of those simulations,
  ;  return #t. Otherwise return #f.
  ; Update: if the trace is linearizable, return true. If the trace is not linearizable,
  ;  for now return false. If the trace is "infeasible", return a list of variables
  ;  that show this. (This list is not very useful, but at least differentiates
  ;  between true and false.
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
                                          (None)
                                          2 2 0)
                                         1 1 0)
                                        0 0 0))
                           (for/list ([var variables])
                             (Create-var (car var) (cdr var)))))
     (define vars (cons mut-ret (map car variables)))

     ; Run the instrumented method and get the results.
     (define results (interpret (append declarations instrs) vars))
     ;(displayln results) (displayln "")
     
     (define infeasible-witnesses
       (for/list ([(var value) results]
                  #:when (void? value))
         var))
     
     (cond
       [(empty? infeasible-witnesses)
        ; Generate all possible interleavings of mut (as an atomic unit) with client.
        ;  (i.e. try inserting mut before, between each pair of instructions, and after.)
        (define other-results '())
        (ormap
         (λ (split-index)
           (let-values ([(before after) (split-at client split-index)])
             (define this-result
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
                vars))
             (set! other-results (cons this-result other-results))
             (equal?
              this-result
              results)))
         (range (add1 (length client))))]

       ; Couldn't find a good variable assignment.
       [else infeasible-witnesses])]))
