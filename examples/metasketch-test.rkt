 #lang racket
(require "../program_representation/simulator-structures.rkt")
(require "../utilities/utilities.rkt")
(require "../examples/mooly-example.rkt")
(require "../examples/mooly-library.rkt")

(require racket/string)

(require "../cex_generalization/to-sketch.rkt")
(require "../cex_generalization/metasketcher.rkt")

(require "../error_trace_generation/error-trace.rkt")
(require (only-in "../error_trace_generation/linearizable.rkt" lin-result-trace))
(require (only-in "../error_trace_generation/methods.rkt" number-lines))
(require "../optimal_cover/cover.rkt" )



(define metasketch-lib ;; (metasketch-library-add-announcement library "extension"))
  library)
  ;; (modify-library-for-optimistic
  ;;  library
  ;;  "extension"
  ;;  (Hole 6 `() 7)))
  





(bound 1)
(define-values (err-traces numbered-method)
  (error-traces
   library
   "extension"

   `(,(Create-var "shared" "Node")
     ,(Create-var "primitive" "int")
     ,(Set-var "shared" (New-struct "Node" `(,(None) 0 0 0)))
     ,(Run-method "push" `(,(Get-var "shared") 1 2) null)
     ,(Run-method "push" `(,(Get-var "shared") 2 4) null))))


(define lib-with-nums (replace-lib-method library "extension" numbered-method))

(define results  err-traces)
(define result-trace-lists
  (map (lambda (t) (lin-result-trace t)) results))



(define hole-set (optimal-cover (map lin-result-trace results) null))




(define (next-method-call t)
  (cond
    [(empty? t) null]
    [(Run-method? (first t)) (first t)]
    [else
     (next-method-call (rest t))]))
     

(define (find-matching-trace traces hole)


  
  ;; Assumes interruption is one line
  (define (matches-hole t)
    (cond
      [(empty? t) #f]
      [(and
        (equal? (hole-before hole) (C-Instruction-instr-id (first t)))
        (not (null? (next-method-call (rest t))))
        (equal? (hole-interrupt hole) (Run-method-method (next-method-call (rest t)))))
       ;; (display "found interruptor: ") (displayln (Run-method-method (next-method-call (rest t))))
       #t]
      [else
       (matches-hole (rest t))]))
       
  
  (let ([matchings (filter matches-hole traces)])
    (cond
      [(empty? matchings) `()]
      [else
       (first matchings)])))



(define (convert-abstract-hole-to-concrete t hole)
  (cond
    [(empty? t) null]
    [(and
      (equal? (hole-before hole) (C-Instruction-instr-id (first t)))
      (not (null? (next-method-call (rest t))))
      (equal? (hole-interrupt hole) (Run-method-method (next-method-call (rest t)))))
     (Hole (hole-before hole) (next-method-call (rest t)) (hole-after hole))]
    [else
     (convert-abstract-hole-to-concrete (rest t) hole)]))
     
      


;; (display "Hole: ") (displayln  (first hole-set))

;; To repair a hole we need a witness to that hole
(define witness-trace (find-matching-trace result-trace-lists (first (second hole-set))))
;; (define witness-trace2 (find-matching-trace result-trace-lists (fourth (first hole-set))))

;; (displayln "found witness traces")
;; (display "witness-trace: ") (displayln witness-trace)
;; (display "witness-trace2: ") (displayln witness-trace2)

;; We then need to get a concrete instance of the hole - confusingly called a Hole
(define concrete-hole (convert-abstract-hole-to-concrete witness-trace (first (second hole-set))))
;; (define concrete-hole2 (convert-abstract-hole-to-concrete witness-trace2 (fourth (first hole-set))))

;; (displayln "found concrete holes from: ")
;; (display "original set: ") (displayln (first hole-set))
;; (display "concrete hole: ") (displayln concrete-hole)
;; (display "concrete hole2: ") (displayln concrete-hole2)


(define optimistic-lib
  (modify-library-for-optimistic
   lib-with-nums
   "extension"
   concrete-hole))

;; (displayln "patched hole1")
;; (define optimistic-two-hole
;;   (modify-library-for-optimistic
;;    optimistic-lib
;;    "extension"
;;    concrete-hole2))
   
;; (displayln "patched hole2")



;; (display "Optimistic method: ")
;; (map (lambda (l) (displayln l))  (Method-instr-list (get-lib-method optimistic-two-hole "extension")))






;; (display "Witness trace for ") (display (first (first hole-set))) (display ": ")
;; (displayln witness-trace)




;; (display "Original traces: ") (displayln (length result-trace-lists))

(define witness-trace-set (list witness-trace)) ;; witness-trace2))
(define all-equivalent-traces

  (expand-traces-to-sketch-lib
   witness-trace-set
   optimistic-lib
   "extension"))




;; (display "equivalent traces: ")
;; (displayln (length all-equivalent-traces))


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

(define TRACE-TYPE (void))
(set! TRACE-TYPE \"no-optimistic-restart\")



(define RETURN-VAL (void))
(define TMP-RET (void))
(define-symbolic meta-var1 boolean?) ;; TODO collect meta-vars
(define OPTIMISTIC (void)) ;; TODO Need to do this automatically

(define-symbolic pick-trace integer?)")


(define initialize-data-structure
  "(METHOD-push (list (list-ref first-args 0) (list-ref first-args 1) 5))")


;; (display "All equivalent: ") (displayln (first all-equivalent-traces))

(define feasible-traces (filter meta-vars-consistent? all-equivalent-traces))
;; (display "Number of traces: ") (displayln (length feasible-traces))
;; (displayln (first feasible-traces))
;; (displayln "")
;; (displayln (second feasible-traces))
;; (displayln "")
;; (displayln (third feasible-traces))
;; (displayln "")
;; (displayln (fourth feasible-traces))



(define all-optimistic-conditions (collect-all-optimistic-expressions feasible-traces))


;; (display "all opt conds: ") (displayln all-optimistic-conditions)
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
   (map (lambda (i) (string-append "(define POSSIBLE" (~v i) " (void))\n (set! POSSIBLE" (~v i) " #t)\n(define META-CHOICE" (~v i) " #t)"))
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

         (displayln "(display \"Trace: \")(displayln pick-trace)")











         (displayln
          (string-replace
           (instr-list-to-sketch which-trace optimistic-lib "first-args" 0 0) 
          ;; (string-replace (instr-list-to-sketch which-trace optimistic-lib "first-args" 0  0) "POSSIBLE" (string-append "POSSIBLE" (~v trace-counter)))
          "META-CHOICE" (string-append "META-CHOICE" (~v trace-counter))))
          

         ;; (display "(display \"POSSIBLE: \") (displayln POSSIBLE)\n")


         (displayln "
(display \"possible: \") (displayln POSSIBLE)
(display \"TRACE-TYPE: \") (displayln TRACE-TYPE)

]")

       )
       traces)


  (displayln ")")


  (displayln (string-append "(assert (and (> pick-trace 0) (< pick-trace " (~v (+ 1 trace-counter)) ")))"))


(displayln (string-append "
(define (things-to-assert)
  (assert (or
           (equal? TRACE-TYPE \"optimistic-restart\")
           (equal? TRACE-TYPE \"broke-out\")
           (equal? TRACE-TYPE \"no-optimistic-restart\")))
  (assert
   (or
    (or (not (equal? TRACE-TYPE \"optimistic-restart\")) POSSIBLE)
    (or (not (equal? TRACE-TYPE \"broke-out\")) POSSIBLE)))
  (assert (or (not (equal? TRACE-TYPE \"no-optimistic-restart\")) (not POSSIBLE))))"))






  
  (displayln "(print-forms (synthesize #:forall (list pick-trace)
                          #:guarantee (things-to-assert)))")

  )






;; (second feasible-traces)

;; (length feasible-traces)


 ;; (map (lambda (i) (if (C-Instruction? i) (begin (display (C-Instruction-instr-id i)) (display ": ") (displayln i)) (displayln "label"))) (second feasible-traces))




(define spit-out (spit-out-traces ;; (list (second feasible-traces))
                  feasible-traces
                  (Hole 1 (list) 2)
                  
                  (list
                   (list (cons "RETURN-VAL" 5) (cons "push5-1" (None)))
                   (list (cons "RETURN-VAL" (None)) (cons "push5-1" 5)))))


 
  (exit)










;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;









(define (hole-found? instr-list hole)
  (cond
    [(empty? instr-list) #f]
    [(equal? (C-Instruction-instr-id (first instr-list)) (Hole-method1 hole))
     #t]
    [else
     (hole-found? (rest instr-list) hole)]))


;; Takes a hole along with a library and the name of a target method in the library
;; Returns a witness trace to that hole
(define (find-witness library method-name hole)

  (define (find-witness-helper instr-list)
    (cond
      [(empty? instr-list) `()]
      [(Single-branch? (first instr-list))
       (if (hole-found? (Single-branch-branch (first instr-list)) hole)
           (append (list (Assume-simulation (Single-branch-condition (first instr-list)) #f))
                   (find-witness-helper (append (Single-branch-branch (first instr-list)) (rest instr-list))))
           (append (list (Assume-simulation (Not (Single-branch-condition (first instr-list))) #f))
                   (find-witness-helper (rest instr-list))))]
      [(Branch? (first instr-list))
       ;; TODO
       (displayln "UH OH - haven't done branches yet")]

      [else
       (if (equal? (C-Instruction-instr-id (first instr-list)) (Hole-method1 hole))
           (append (list (first instr-list)) (Hole-interruptor hole) (rest instr-list))
           (append (list (first instr-list)) (find-witness-helper (rest instr-list))))]
       ;; (display "Found: ") (displayln (first instr-list))
       ;; (displayln "UNEXPECTED: REACHED ELSE CASES")]
      

    
    )

    (let ([method-instr-list (Method-instr-list (get-lib-method library method-name))])
      (find-witness-helper method-instr-list)))

  (let ([m (Method-instr-list (get-lib-method library method-name))])
    (find-witness-helper  m)))
    

    




  




;; (find-witness library "extension" (first (first hole-set)))












;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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






