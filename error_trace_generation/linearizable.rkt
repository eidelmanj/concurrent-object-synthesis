#lang racket/base

(require (only-in "../program_representation/simulator-structures.rkt"
                  Method Create-var Run-method)
         (only-in racket/list first)
         (only-in racket/match match)
         (only-in racket/pretty pretty-display)
         "interpret.rkt"
         "vars.rkt")

(provide linearizable?)

(define (linearizable? trace mut client variables pointers library interpret)
  ; Simulate each possible interleaving of mut with the instructions of client.
  ; If the result of running the trace matches the result of one of those simulations,
  ;  return #t. Otherwise return #f.
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
     (define declarations (for/list ([var variables])
                            (Create-var (car var) (cdr var))))
     (define vars (cons mut-ret (map car variables)))

     ; Run the instrumented method and get the results.
     (define results (interpret (append declarations instrs) vars))

     (displayln results)])
     
     ;(error "test")])
     
     
     ; Generate all possible interleavings of mut (as an atomic unit) with client.
     ;  (i.e. try inserting mut before, between each pair of instructions, and after.)
     #;(for/list ([i (add1 (length client))])
         (let-values ([(before after) (split-at client i)])
           (interleaving-to-sketch
            (Thread-list (append declarations before trace after))
            vars
            (cons old-mut library))))

  #f)