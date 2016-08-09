 #lang racket
(require "../program_representation/simulator-structures.rkt")
(require "../utilities/utilities.rkt")
(require "../examples/mooly-example.rkt")
(require "../examples/mooly-library.rkt")
(require racket/string)

(require "../cex_generalization/to-sketch.rkt")
(require "../cex_generalization/metasketcher.rkt")


(define metasketch-lib ;; (metasketch-library-add-announcement library "extension"))
  library)
  ;; (modify-library-for-optimistic
  ;;  library
  ;;  "extension"
  ;;  (Hole 6 `() 7)))
  

(define error-trace-example

  (list

 (Create-var "val" "int")
 (Create-var  "found" "int")
 (Create-var "throwaway" "int")

 (Label "START")
 (Set-var "val" (None))
 (Run-method

  "contains"
  (list (Get-argument 0) (Get-argument 1))
  "found")
 (Assume-simulation (Get-var "found"))
 (Run-method

  "get"
  (list (Get-argument  0) (Get-argument 1))
  "val")
 (Run-method   "remove" (list (Get-var "shared") 1) 'push5-1)
 (Run-method

  "remove"
  (list (Get-argument   0) (Get-argument   1))
  "throwaway")

 (Return   (Get-var "val"))))






;; Give ids to the trace example


(set-C-Instruction-thread-id! (list-ref error-trace-example 8) #t) 



(define (assign-ids l n)
  (cond
    [(empty? l) void]
    [(Single-branch? (first l))
     (assign-ids (Single-branch-branch (first l)) n)]
    [else
     (if (and (not (Assume-simulation? (first l))) (C-Instruction? (first l)) (null? (C-Instruction-thread-id (first l))))
         (begin 
           (assign-ids (rest l) (+ n 1))
           (set-C-Instruction-instr-id! (first l) n))
         (assign-ids (rest l) n))]))

(assign-ids error-trace-example 0)



;; error-trace-example

;; Give corresponding ids to the extension itself

;; (displayln metasketch-lib)

(assign-ids (Method-instr-list (fifth metasketch-lib)) 0)
(set-C-Instruction-instr-id! (last (Method-instr-list (fifth metasketch-lib))) 7)

;; error-trace-example
;; (Method-instr-list (first metasketch-lib))
;; metasketch-lib

(define optimistic-lib
  (modify-library-for-optimistic
   library
   "extension"
   (Hole 5 `() 6)))


(define all-equivalent-traces

  (expand-traces-to-sketch-lib
   (list error-trace-example)
   optimistic-lib
   "extension"))





  (define prelude "
    
#lang rosette
(require rosette/lib/synthax)

(define current-thread (void))

(define method-exit (void))
(set! method-exit #f)


(define lock-list (void))
(set! lock-list (list))



(define (has-lock t-id lock)
  (cond
    [(> lock (- (length lock-list) 1))
     (set! lock-list (append lock-list (list t-id)))
     #t]
    [else
     (or (> 0 (list-ref lock-list lock)) (equal? t-id (list-ref lock-list lock)))]))



(define (replace-lock lock t-id)
  (cond
    [(equal? lock 0)
     (append (list t-id) (rest lock-list))]
    [else
     (append (list (first lock-list)) (replace-lock (- lock 1) t-id))]))

(define (get-lock t-id lock)
  (set! lock-list (replace-lock lock t-id)))

(define (release-lock t-id lock)
  (set! lock-list (replace-lock lock -1)))


(struct Node (next key val bits) #:mutable)

(define shared (void))
(struct None ())
(set! shared (Node  (None) \"test\" \"testval\" (None)))


(define first-args (void))
(set! first-args (list shared 1))
(define POSSIBLE (void))
(set! POSSIBLE #t)



(define RETURN-VAL (void))
(define TMP-RET (void))
(define-symbolic meta-var1 boolean?) ;; TODO collect meta-vars
(define OPTIMISTIC (void)) ;; TODO Need to do this automatically

(define-symbolic pick-trace integer?)")


(define initialize-data-structure
  "(METHOD-push (list (list-ref first-args 0) (list-ref first-args 1) 5))")


(define feasible-traces (filter meta-vars-consistent? all-equivalent-traces))

(define all-optimistic-conditions (collect-all-optimistic-expressions feasible-traces))


(define opt-cond-declarations (generate-optimistic-condition-sketches all-optimistic-conditions 3))







(define (linearizable-solutions-to-assertions lin-solutions)
  (define (handle-single-solution sol)
    (string-append "(and \n"
                   (reduce
                    string-append
                    (map (lambda (s)
                           (cond
                             [(None? (cdr s))
                              (string-append "(None? " (car s) ")")]
                             [else
                              (string-append "(equal? " (car s) " " (~v (cdr  s)) ")\n ")]))
                         sol))
                   ")\n"))

  (string-append "(or \n"
  (reduce
   string-append
   (map (lambda (sol)
          (handle-single-solution sol))
        lin-solutions))
  ")\n"))




(define trace-counter (void))
(set! trace-counter 0)

(define (spit-out-traces traces hole lin-solutions)



  (define interfering-var-declarations
    (reduce
     string-append
     (unique
      equal?
      (reduce
       append
       (map
        (lambda (which-trace)
          (reduce
           append
           (map (lambda (ti) (list (string-append "(define " (format "~a" ti) " (void))\n")))
                (get-interfering-ret-vars which-trace))))
        traces)))))






  (display prelude)

  (displayln (reduce
   string-append
   (map (lambda (i) (string-append "(define POSSIBLE" (~v i) " (void))\n (set! POSSIBLE" (~v i) " #t)\n"))
        (range (length traces)))))


  (displayln interfering-var-declarations)
  (display  (generate-library-code optimistic-lib))
  (displayln initialize-data-structure)

  (displayln opt-cond-declarations)


  (displayln "(cond ")
  
  (map (lambda (which-trace)
         (set! trace-counter (+ trace-counter 1))
  
         ;; (map (lambda (l) (displayln l)) which-trace)

         
         (displayln (string-append "[(equal? pick-trace " (~v trace-counter) ")"))











         (display (string-replace (instr-list-to-sketch which-trace optimistic-lib "first-args" 0  0) "POSSIBLE" (string-append "POSSIBLE" (~v trace-counter))))

         ;; (display "(display \"POSSIBLE: \") (displayln POSSIBLE)\n")


         (if (optimistic-stopped-hole? which-trace hole)
             (begin (displayln ";; THIS TRACE IS MANDATORY")
             (displayln (string-append "(assert POSSIBLE" (~v trace-counter) ")")))
             (displayln ""))
         (displayln (string-append "(display \"POSSIBLE" (~v trace-counter) ":\" ) (displayln POSSIBLE" (~v trace-counter) ")"))
         (displayln (string-append "(assert (or (not POSSIBLE" (~v trace-counter)")"))
         (displayln (linearizable-solutions-to-assertions lin-solutions))
         (displayln "))")
         (displayln "]")

       )
       traces)


  (displayln ")")

  (displayln (string-append "(assert (and (> pick-trace 0) (< pick-trace " (~v (+ 1 trace-counter)) ")))"))



  
  (displayln "(print-forms (synthesize #:forall (list pick-trace)
                          #:guarantee (asserts)))"))






;; (second feasible-traces)

;; (length feasible-traces)


 ;; (map (lambda (i) (if (C-Instruction? i) (begin (display (C-Instruction-instr-id i)) (display ": ") (displayln i)) (displayln "label"))) (second feasible-traces))




(define spit-out (spit-out-traces ;; (list (second feasible-traces))
                  feasible-traces
                                  (Hole 5 (list) 6)

                                  (list
                                   (list (cons "RETURN-VAL" 5) (cons "push5-1" (None)))
                                   (list (cons "RETURN-VAL" (None)) (cons "push5-1" 5)))))


 



