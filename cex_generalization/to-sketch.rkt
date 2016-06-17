#lang racket 
(require "../program_representation/simulator-structures.rkt")

(require racket/string)

(provide
 to-string-instr
 print-non-sketch-simulation
 retrieve-code
 reduce
 instr-list-to-sketch)

(define (reduce func list)
  (if (null? (cdr list))
      (car list)
      (func (car list) (reduce func (cdr list)))))


(define (retrieve-code library id)
  (let ([lib-matches (filter (lambda (m) (equal? (Method-id m) id)) library)])
    (Method-instr-list (first lib-matches))))


(define num-sim-loops (void))
(set! num-sim-loops 0)


(define (new-sim-loop-name)
  (set! num-sim-loops (+ num-sim-loops 1))
  (string-append "loop" (~v num-sim-loops)))



(define num-arg-lists (void))
(set! num-arg-lists 0)
(define (new-arg-list-id)
  (set! num-arg-lists (+ 1 num-arg-lists))
  (string-append "arg-list" (~v num-arg-lists)))

(define (print-non-sketch-simulation instr-list library arg-store ret-store)
  (cond
    [(empty? instr-list) ""]
    [else
     (let ([elem (first instr-list)])
       (cond
         [(Loop? elem)
          (let ([loop-name (new-sim-loop-name)])
            (string-append
             "(define (" loop-name " c )\n"
             "(if c\n"
             "(begin\n"
             (print-non-sketch-simulation (Loop-instr-list elem) library arg-store ret-store)
             "(" loop-name " " (to-string-instr (Loop-condition elem) arg-store) "))\n"
             "(begin (void))))\n"
             "(" loop-name " " (to-string-instr (Loop-condition elem) arg-store) ")\n"
             (print-non-sketch-simulation (rest instr-list) library arg-store ret-store)
             ))]


         [(Meta-addition? elem)
          (string-append
           "(if meta-var" (~v (Meta-addition-which-var elem)) "\n"
           "(begin\n"
           (print-non-sketch-simulation (Meta-addition-instr-list elem) library arg-store ret-store)
           ")\n"

           "(begin (void)))\n"
           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store)
           )]



           
         [(Repeat-meta? elem)
          (string-append
           "(cond "
           (sketch-unroll-repeat-non-simul (Repeat-meta-instr-list elem) (string-append "meta-var" (~v (Repeat-meta-which-var elem)))  3 library arg-store ret-store)
           ")"

           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store))]

          


         [(Single-branch? elem)
          (string-append
           "(if " (to-string-instr (Single-branch-condition elem) arg-store ) "\n"
           "(begin\n"
           (print-non-sketch-simulation (Single-branch-branch elem) library arg-store ret-store)
           ")\n"
           "(begin (void)))"

           "(if method-exit\n"
           "(begin\n"
           "(set! method-exit #f)\n"
           ")\n"
           "(begin\n"
           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store)
           "(void)\n"
           "))\n"
           )]

         [(Branch? elem)
          (string-append
           "(if " (to-string-instr (Branch-condition elem) arg-store) "\n"
           "(begin\n"
           (print-non-sketch-simulation (Branch-branch1 elem) library arg-store ret-store)
           ")\n"
           "(begin\n"
           (print-non-sketch-simulation (Branch-branch2 elem) library arg-store ret-store)
           "))"
           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store)
           )]


         [(Run-method? elem)
          (let ([args-list (new-arg-list-id)])
            (string-append
             "(set! current-thread " (~v (C-Instruction-thread-id elem)) ")\n"
             "(define " args-list " (void))\n"
             "(set! " args-list " " (to-string-instr (Run-method-args elem) arg-store) ")\n"
             "(begin\n"
             (print-non-sketch-simulation (retrieve-code library (Run-method-method elem)) library args-list (Run-method-ret elem))
             ")\n"
             (print-non-sketch-simulation (rest instr-list) library args-list ret-store)
             ))]
           
          
          

         
         [(Create-var? elem)
          ;; (display "create-var-id: ") (display (Create-var-id elem)) (display "\n")
          (string-append
           "(display \"creating.... " (Create-var-id elem) "\n\")"
           "(define " (Create-var-id elem) " (void))\n"
           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store))]


         [(Lock? elem)
          (string-append "(if (not (has-lock current-thread " (~v (Lock-id elem)) " ))\n"
                         "#f\n"
                         "(begin \n (get-lock current-thread " (~v (Lock-id elem)) ")\n" "(display \"locking!\n\")"
                         (print-non-sketch-simulation (rest instr-list) library arg-store ret-store)
                         "))")]

         [(Unlock? elem)
          (string-append "(release-lock current-thread " (~v (Unlock-id elem)) ")\n"
                         (print-non-sketch-simulation (rest instr-list) library arg-store ret-store))]

         [(Set-var? elem)
          ;; "todo\n"]
          ;; (display "found set var\n")
          (string-append
           "(set! " (Set-var-id elem) " " (to-string-instr (Set-var-assignment elem) arg-store) ")\n"
           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store))]



         [(Set-pointer? elem)
          (string-append
           "(set-" (Set-pointer-type elem) "-" (Set-pointer-offset elem) "! " (Set-pointer-id elem) " "  (to-string-instr (Set-pointer-val elem) arg-store) ")\n"
           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store))]


         [(CAS? elem)
          (string-append

           "(if (not (equal? " (to-string-instr (CAS-v1 elem) arg-store) " " (to-string-instr (CAS-v2 elem) arg-store) "))\n"
           "(begin\n "
           "(" (special-set-string-instr (CAS-v1 elem) arg-store) " " (to-string-instr (CAS-new-val elem) arg-store) ")\n"
           ;; "(set! " (special-set-string-instr (CAS-v1 elem) arg-store) " " (to-string-instr (CAS-new-val elem) arg-store) ")\n"
           "(set! " (CAS-ret elem) " 1))"
           "(begin\n "
           "(set! " (CAS-ret elem) " 0)))\n"

           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store))]
           

         [(Continue? elem)
          (let ([where-to (find-continue-sublist (rest instr-list) (Continue-to-where elem))])
            ;; (display (Continue-to-where elem)) (display "\n")

            "(void)")]
             ;; (print-non-sketch-simulation `() library arg-store ret-store))]


         [(Return? elem)
          (string-append
           "(display \"returning....."  (to-string-instr (Return-val elem) arg-store) "\n\")\n"
           "(set! " ret-store " " (to-string-instr (Return-val elem) arg-store) ")\n"
           "(set! method-exit #t)\n"
          )]
           ;; (print-non-sketch-simulation (rest instr-list) library arg-store ret-store))]
                         


         [else
          ;; (display "todo case\n")
          (string-append
           ;; "TODO\n"
           ";;TODO:"
           (~v (first instr-list)) "\n"
           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store))]))]))
  



(define (special-set-string-instr instr arg-store)
  (cond
    [(Dereference? instr)
     (string-append
      "set-" (Dereference-type instr) "-" (Dereference-offset instr) "! " (Dereference-id instr))]
    [else
     (string-append
      "set! " (to-string-instr instr arg-store))]))

(define (to-string-instr instr arg-store)
  ;; (display "to-string: ") (display instr) (display "\n")
  (cond
    [(Dereference? instr)
     (cond
       [(Dereference? (Dereference-id instr))
        (string-append
         "("(Dereference-type instr) "-" (Dereference-offset instr) " " (to-string-instr (Get-var (to-string-instr (Dereference-id instr) arg-store)) arg-store) ")")]
        [else

         (string-append
          "("(Dereference-type instr) "-" (Dereference-offset instr) " "  (Dereference-id instr) ")")])]
    [(Equal? instr)
     (string-append "(equal? " (to-string-instr (Equal-expr1 instr) arg-store) " " (to-string-instr (Equal-expr2 instr) arg-store) ")")]
    [(Or? instr)
     (string-append "(or " (to-string-instr (Or-expr1 instr) arg-store) " " (to-string-instr (Or-expr2 instr) arg-store) ")")]
    [(Not? instr)
     (string-append "(not " (to-string-instr (Not-expr instr) arg-store) ")")]
    [(And? instr)
     (string-append "(and " (to-string-instr (And-expr1 instr) arg-store) " " (to-string-instr (And-expr2 instr) arg-store) ")")]
    [(Get-var? instr)
     (string-append (Get-var-id instr))]
    [(Get-argument? instr)
     (string-append "(list-ref " arg-store " " (~v (Get-argument-id instr)) ")")]

    [(list? instr)
     (string-append "(list " (reduce (lambda (a b) (string-append a " " b)) (map (lambda (i) (to-string-instr i arg-store)) instr)) ")")]

    [(Is-none?? instr)
     (string-append "(None? " (to-string-instr (Is-none?-val instr) arg-store) ")")]




    [(Mystery-const? instr)
     (string-append "??")]
                    

    [else
 
     (~v instr)]))



(define (list-multiply instr-list num)
  (cond
    [(equal? num 0) `()]
    [else
     (append instr-list (list-multiply instr-list (- num 1)))]))

(define (sketch-unroll-repeat instr-list meta-var depth arg-store)
  (cond
    [(equal? depth 0) (string-append "[(equal? " meta-var " 0) (begin (void))]\n")]
    [else
     (string-append
      "[(equal? " meta-var " " (~v depth) ") (begin \n" (instr-list-to-sketch (list-multiply instr-list depth) arg-store) ")]\n"
      (sketch-unroll-repeat instr-list meta-var (- depth 1) arg-store))]))



(define (sketch-unroll-repeat-non-simul instr-list meta-var depth library arg-store ret-store)
  (cond
    [(equal? depth 0) (string-append "[(equal? " meta-var " 0) (begin (void))]\n")]
    [else
     (string-append
      "[(equal? " meta-var " " (~v depth) ") (begin \n" (print-non-sketch-simulation (list-multiply instr-list depth) library arg-store ret-store)  ;; (instr-list-to-sketch (list-multiply instr-list depth) arg-store)
      ")]\n"
      (sketch-unroll-repeat-non-simul instr-list meta-var (- depth 1) library arg-store ret-store))]))
     
      



  



(define (instr-list-to-sketch instr-list library arg-store)
  ;; (display "to-sketch: ")(display (first instr-list))(display "\n")
  (cond
    [(empty? instr-list) ""]
    [else
     (let ([elem (first instr-list)])
       (cond
         [(Create-var? elem)
          ;; (display "create-var-id: ") (display (Create-var-id elem)) (display "\n")
          (string-append
           "(define " (Create-var-id elem) " (void))\n"
           (instr-list-to-sketch (rest instr-list) library arg-store))]


         [(Lock? elem)
          (string-append "(if (not (has-lock current-thread " (~v (Lock-id elem)) " ))\n"
                         "#f"
                         "(begin \n"
                         (instr-list-to-sketch (rest instr-list) library arg-store)
                         "))")]




         [(Run-method? elem)
          (let ([args-list (new-arg-list-id)])
            (string-append
             "(set! current-thread " (~v (C-Instruction-thread-id elem)) ")\n"
             "(define " args-list " (void))\n"
             "(set! " args-list " " (to-string-instr (Run-method-args elem) arg-store) ")\n"
             "(begin\n"
             (print-non-sketch-simulation (retrieve-code library (Run-method-method elem)) library args-list (Run-method-ret elem))
             ")\n"
             (instr-list-to-sketch (rest instr-list) library arg-store)
             ;; (print-non-sketch-simulation (rest instr-list) library args-list ret-store)
             ))]

         [(Set-var? elem)
          ;; "todo\n"]
          ;; (display "found set var\n")
          (string-append
           "(set! " (Set-var-id elem) " " (to-string-instr (Set-var-assignment elem) library arg-store) ")\n"
           (instr-list-to-sketch (rest instr-list) library arg-store))]

         [(Assume-simulation? elem)
          (string-append
           "(if " (to-string-instr (Assume-simulation-condition elem) library arg-store) "\n #f\n (begin \n"
           (instr-list-to-sketch (rest instr-list) library arg-store) ""
           "))\n")]

         [(Assume-loop? elem)
          (string-append
           "(if " (to-string-instr (Assume-loop-condition elem) library arg-store) "\n #f\n (begin \n"
           (instr-list-to-sketch (rest instr-list) library arg-store) ""
           "))\n")]


         [(Set-pointer? elem)
          (string-append
           "(set-" (Set-pointer-type elem) "-" (Set-pointer-offset elem) "! " (Set-pointer-id elem) " "  (to-string-instr (Set-pointer-val elem) library arg-store) ")\n"
           (instr-list-to-sketch (rest instr-list) library arg-store))]

         [(Assume-meta? elem)
          (string-append
           "(if (not meta-var)\n #f (begin  \n"
           (instr-list-to-sketch (rest instr-list) library arg-store) "))\n")]

         [(Repeat-meta? elem)
          (string-append
           "(cond "
           (sketch-unroll-repeat (Repeat-meta-instr-list elem) "meta-count" 3 library arg-store)
           ")"
           
           (instr-list-to-sketch (rest instr-list) library arg-store))]


         [(CAS? elem)
          (string-append

           "(if  (not (equal? " (to-string-instr (CAS-v1 elem) library arg-store) " " (to-string-instr (CAS-v2 elem) library arg-store) "))\n"
           "(begin\n "
           "(set! " (to-string-instr (CAS-v1 elem) library arg-store) " " (to-string-instr (CAS-new-val elem) library arg-store) ")\n"
           "(set! " (CAS-ret elem) " 1))"
           "(begin\n "
           "(set! " (CAS-ret elem) " 0)))\n"

           (instr-list-to-sketch (rest instr-list) library arg-store))]
           

         [(Continue? elem)
          (let ([where-to (find-continue-sublist (rest instr-list) (Continue-to-where elem))])
            ;; (display (Continue-to-where elem)) (display "\n")
            (instr-list-to-sketch where-to library arg-store))]



         [else
          ;; (display "todo case\n")
          (string-append
           ;; "TODO\n"
           ";;TODO:"
           (~v (first instr-list)) "\n"
           (instr-list-to-sketch (rest instr-list) library arg-store))]))]))
           


         
(define (find-continue-sublist instr-list to-where)
  ;; (display "finding continue: ") (display instr-list) (display to-where) (display "\n")
  (cond
    [(empty? instr-list) `()]
    [(Assume-loop? (first instr-list))
     (cond
       [(equal? to-where (Assume-loop-to-where (first instr-list)))
        instr-list]
       [else
        (find-continue-sublist (rest instr-list) to-where)])]
    [else (find-continue-sublist (rest instr-list) to-where)]))



;; (display (instr-list-to-sketch (list
;;                                 (Create-var "test" "Node" (None))
;;                                 (Set-var "test" (Dereference "cur" "Node" "next" ) (None))
;;                                 (Assume-simulation (Or (Equal (Get-var "test" ) 1) (Equal (Get-var "test" ) 2)))
;;                                 (Set-pointer "test" "Node" "next" (Dereference "cur" "Node" "next") (None))

;;                                 )))
     




;; (struct node (key val bits next) #:mutable #:transparent)

;; (struct Null-node ())
;; (struct Null-val ())

;; (define n (void))
;; (set! n (Null-node))

;; (define (new-node key val)
;;   (node key val 0 (Null-node)))

;; (define (set-next n next)
;;   (set-node-next! n next))

;; (set! n (new-node "a" 1))

;; (set-next n (new-node "b" 2))
;; n
;; (set-next n (new-node "c" 3))
;; n
