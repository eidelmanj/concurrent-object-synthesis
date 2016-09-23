#lang racket 
(require "../program_representation/simulator-structures.rkt")
(require "../utilities/utilities.rkt")

(require racket/string)
(require racket/hash)

(provide
 to-string-instr
 print-non-sketch-simulation
 add-binding-parent
 retrieve-code
 new-scope-num
 generate-optimistic-condition-lists
 generate-optimistic-condition-sketches
 get-interfering-ret-vars
 generate-library-code
 generate-smart-optimistic-condition-grammar
 generate-top-level-grammar
 trace-list-to-sketch
 instr-list-to-sketch)




;; Find all the return variables of interfering methods
;; so that we can initialize them in sketch
(define (get-interfering-ret-vars t)
  (let ([interfering-lines
         (filter
          (lambda (ti)
            (and (Run-method? ti) (boolean? (C-Instruction-thread-id ti))))
          t)])
    (unique
     equal?
     (map
     (lambda (ti) (Run-method-ret ti))
     interfering-lines))))



;;;;;;;;;;;;;;;;; Binding related stuff - TODO: Determine whether still needed ;;;;;;;;;;;;;;;;;;;;;;
(define num-sim-loops (void))
(set! num-sim-loops 0)

(define scope-count (void))
(set! scope-count 0)
(define (new-scope-num)
  (set! scope-count (+ scope-count 1))
  scope-count)

(define (new-sim-loop-name)
  (set! num-sim-loops (+ num-sim-loops 1))
  (string-append "loop" (~v num-sim-loops)))



(define num-arg-lists (void))
(set! num-arg-lists 0)
(define (new-arg-list-id)
  (set! num-arg-lists (+ 1 num-arg-lists))
  (string-append "arg-list" (~v num-arg-lists)))



(define all-bindings (make-hash))

(define (new-binding id scope-num parent-scope)
  (let ([id-bindings (hash-ref all-bindings scope-num)])
    (hash-set! all-bindings scope-num
               (Binding-list
                (Binding-list-parent id-bindings)
                (append (Binding-list-id-list id-bindings) (list id))))))


    
(define (add-binding-parent scope-num parent-scope)
  (hash-set! all-bindings scope-num (Binding-list parent-scope (list))))

(define (list-contains l elem)
  (> (length (filter (lambda (i) (equal? i elem)) l)) 0))

(define (get-most-recent-binding var-name scope-num parent-scope)
  (cond
    [(equal? scope-num 0) 0]
    [else
     (let ([bindings (hash-ref all-bindings scope-num)])
       (let ([parent-binding (Binding-list-parent bindings)]
             [id-list (Binding-list-id-list bindings)])
           
         ;; (display "most recent: ") (display var-name) (display scope-num) (displayln id-list)
         (if (list-contains id-list var-name)
             scope-num
             (get-most-recent-binding var-name parent-binding parent-scope))))]))
    




;;;;;;;;;;;;;;;;;;;;;;;;;;; Trace to Sketch conversion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (print-non-sketch-simulation instr-list library arg-store ret-store scope-num parent-scope)

  (cond
    [(empty? instr-list) ""]
    [else
     ;; (display "FIRST INSTR: ") (displayln instr-list)
     (let ([elem (first instr-list)])
       (cond
         [(Loop? elem)
          (let ([loop-name (new-sim-loop-name)])
            (string-append
             "(define (" loop-name " c )\n"
             "(if c\n"
             "(begin\n"
             (print-non-sketch-simulation (Loop-instr-list elem) library arg-store ret-store scope-num parent-scope)
             "(" loop-name " " (to-string-instr (Loop-condition elem) arg-store scope-num parent-scope) "))\n"
             "(begin (void))))\n"
             "(" loop-name " " (to-string-instr (Loop-condition elem) arg-store scope-num parent-scope) ")\n"
             (print-non-sketch-simulation (rest instr-list) library arg-store ret-store scope-num parent-scope)
             ))]


         [(Meta-addition? elem)
          (string-append
           "(if meta-var" (~v (Meta-addition-which-var elem)) "\n"
           "(begin\n"
           (print-non-sketch-simulation (Meta-addition-instr-list elem) library arg-store ret-store scope-num parent-scope)
           ")\n"

           "(begin (void)))\n"
           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store scope-num parent-scope)
           )]



           
         [(Repeat-meta? elem)
          (string-append
           "(cond "
           (sketch-unroll-repeat-non-simul (Repeat-meta-instr-list elem) (string-append "meta-var" (~v (Repeat-meta-which-var elem)))  3 library arg-store ret-store scope-num parent-scope)
           ")"

           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store scope-num parent-scope))]

          


         [(Single-branch? elem)
          (string-append
           "(if " (to-string-instr (Single-branch-condition elem) arg-store scope-num parent-scope) "\n"
           "(begin\n"
           (print-non-sketch-simulation (Single-branch-branch elem) library arg-store ret-store scope-num parent-scope)
           ")\n"
           "(begin (void)))"

           "(if method-exit\n"
           "(begin\n"
           "(set! method-exit #f)\n"
           ")\n"
           "(begin\n"
           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store scope-num parent-scope)
           "(void)\n"
           "))\n"
           )]

         [(Branch? elem)
          (string-append
           "(if " (to-string-instr (Branch-condition elem) arg-store scope-num parent-scope) "\n"
           "(begin\n"
           (print-non-sketch-simulation (Branch-branch1 elem) library arg-store ret-store scope-num parent-scope)
           ")\n"
           "(begin\n"
           (print-non-sketch-simulation (Branch-branch2 elem) library arg-store ret-store scope-num parent-scope)
           "))"
           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store scope-num parent-scope)
           )]


         [(Run-method? elem)
         ;;  (let ([args-list (new-arg-list-id)]
         ;;        [new-scope (new-scope-num)])
         ;;    (add-binding-parent new-scope scope-num)
         ;;    (string-append
         ;;     "(set! current-thread " (~v (C-Instruction-thread-id elem)) ")\n"
         ;;     "(set! method-exit #f)\n"
         ;;     "(define " args-list " (void))\n"
         ;;     "(set! " args-list " " (to-string-instr (Run-method-args elem) arg-store scope-num parent-scope) ")\n"
         ;;     "(begin\n"
         ;;     (print-non-sketch-simulation (retrieve-code library (Run-method-method elem)) library args-list (Run-method-ret elem) new-scope scope-num)
         ;;     ")\n"
         ;;     (print-non-sketch-simulation (rest instr-list) library args-list ret-store scope-num parent-scope)
         ;;     ))]

          (string-append
           "(set! " (Run-method-ret elem) " "
           "(METHOD-" (Run-method-method elem) " " (to-string-instr (Run-method-args elem) arg-store scope-num parent-scope) "))\n"

           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store scope-num parent-scope))]
           
          
          

         
         [(Create-var? elem)
          ;; (display "create-var-id: ") (display (Create-var-id elem)) (display "\n")
          ;; (new-binding (Create-var-id elem) scope-num parent-scope)
          (string-append
           ;; "(display \"creating.... " (Create-var-id elem) "\n\")"
           "(define " (Create-var-id elem) ;; (~v (get-most-recent-binding (Create-var-id elem)  scope-num parent-scope))
           " (void))\n"
           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store scope-num parent-scope))]


         [(Lock? elem)
          (string-append "(if (not (has-lock current-thread " (~v (Lock-id elem)) " ))\n"
                         "(set! POSSIBLE #f)\n"
                         "(begin \n (get-lock current-thread " (~v (Lock-id elem)) ")\n" ;; "(display \"locking!\n\")"
                         (print-non-sketch-simulation (rest instr-list) library arg-store ret-store scope-num parent-scope)
                         "))")]

         [(Unlock? elem)
          (string-append "(release-lock current-thread " (~v (Unlock-id elem)) ")\n"
                         (print-non-sketch-simulation (rest instr-list) library arg-store ret-store scope-num parent-scope))]

         [(Set-var? elem)
          ;; "todo\n"]
          ;; (display "found set var\n")
          (string-append
           "(set! " (Set-var-id elem) ;; (~v (get-most-recent-binding (Set-var-id elem) scope-num parent-scope))
           " " (to-string-instr (Set-var-assignment elem) arg-store scope-num parent-scope) ")\n"
           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store scope-num parent-scope))]



         [(Set-pointer? elem)
          (string-append
           "(set-" (Set-pointer-type elem) "-" (Set-pointer-offset elem) "! " (Set-pointer-id elem)  ;; (~v (get-most-recent-binding (Set-pointer-id elem) scope-num parent-scope))
           " "  (to-string-instr (Set-pointer-val elem) arg-store scope-num parent-scope) ")\n"
           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store scope-num parent-scope))]


         [(CAS? elem)
          (string-append

           "(if (not (equal? " (to-string-instr (CAS-v1 elem) arg-store scope-num parent-scope) " " (to-string-instr (CAS-v2 elem) arg-store scope-num parent-scope) "))\n"
           "(begin\n "
           "(" (special-set-string-instr (CAS-v1 elem) arg-store scope-num parent-scope) " " (to-string-instr (CAS-new-val elem) arg-store scope-num parent-scope) ")\n"
           ;; "(set! " (special-set-string-instr (CAS-v1 elem) arg-store) " " (to-string-instr (CAS-new-val elem) arg-store) ")\n"
           "(set! " (CAS-ret elem) ;; (~v (get-most-recent-binding (CAS-ret elem) scope-num parent-scope))
           " 1))"
           "(begin\n "
           "(set! " (CAS-ret elem) ;; (~v (get-most-recent-binding (CAS-ret elem) scope-num parent-scope))
           " 0)))\n"

           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store scope-num parent-scope))]
           

         [(Continue? elem)
          (let ([where-to (find-continue-sublist (rest instr-list) (Continue-to-where elem))])
            ;; (display (Continue-to-where elem)) (display "\n")

            "(void)")]
             ;; (print-non-sketch-simulation `() library arg-store ret-store))]


         [(Return? elem)
          (string-append
           ;; "(display \"returning....."  (to-string-instr (Return-val elem) arg-store scope-num parent-scope) "\n\")\n"
           ;; "(set! " ret-store ;; (~v (get-most-recent-binding ret-store scope-num parent-scope))
           ;; " " (to-string-instr (Return-val elem) arg-store scope-num parent-scope) ")\n"
           "(set! method-exit #t)\n"
           "(set! TO-RETURN " (to-string-instr (Return-val elem) arg-store scope-num parent-scope) " )\n"
           ;; (to-string-instr (Return-val elem) arg-store scope-num parent-scope)
          )]
           ;; (print-non-sketch-simulation (rest instr-list) library arg-store ret-store))]
                         


         [else
          ;; (display "todo case\n")
          (string-append
           ;; "TODO\n"
           ";;TODO:"
           (~v (first instr-list)) "\n"
           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store scope-num parent-scope))]))]))
  



(define (special-set-string-instr instr arg-store scope-num parent-scope)
  (cond
    [(Dereference? instr)
     (string-append
      "set-" (Dereference-type instr) "-" (Dereference-offset instr) "! " (Dereference-id instr) (~v (get-most-recent-binding (Dereference-id instr) scope-num parent-scope)))]
    [else
     (string-append
      "set! " (to-string-instr instr arg-store scope-num parent-scope))]))

(define (to-string-instr instr arg-store scope-num parent-scope)
  ;; (display "to-string: ") (display instr) (display "\n")
  (cond

    [(Optimistic-Condition? instr)
     ;; TODO Need to deal with optimistic condition
     (string-append
      "(OPT" (~v (Optimistic-Condition-meta-var instr)) ")")]


    
    [(Dereference? instr)
     (cond
       [(Dereference? (Dereference-id instr))
        (string-append
         "("(Dereference-type instr) "-" (Dereference-offset instr) " " (to-string-instr (Dereference-id instr) arg-store scope-num parent-scope) ")")]

        ;; (string-append
        ;;  "("(Dereference-type instr) "-" (Dereference-offset instr) " " (to-string-instr (Get-var (to-string-instr (Dereference-id instr) arg-store scope-num parent-scope)) arg-store scope-num parent-scope) ")")]
        [else

         (string-append
          "("(Dereference-type instr) "-" (Dereference-offset instr) " "  (Dereference-id instr) ;; (~v (get-most-recent-binding (Dereference-id instr) scope-num parent-scope))
          ")")])]
    [(Equal? instr)
     (string-append "(equal? " (to-string-instr (Equal-expr1 instr) arg-store scope-num parent-scope) " " (to-string-instr (Equal-expr2 instr) arg-store scope-num parent-scope) ")")]
    [(Or? instr)
     (string-append "(or " (to-string-instr (Or-expr1 instr) arg-store scope-num parent-scope) " " (to-string-instr (Or-expr2 instr) arg-store scope-num parent-scope) ")")]
    [(Not? instr)
     (string-append "(not " (to-string-instr (Not-expr instr) arg-store scope-num parent-scope) ")")]
    [(And? instr)
     (string-append "(and " (to-string-instr (And-expr1 instr) arg-store scope-num parent-scope) " " (to-string-instr (And-expr2 instr) arg-store scope-num parent-scope) ")")]
    [(Get-var? instr)
     (string-append (Get-var-id instr) ;; (~v (get-most-recent-binding (Get-var-id instr) scope-num parent-scope))
                    )]
    [(Get-argument? instr)
     (string-append "(list-ref " arg-store " " (~v (Get-argument-id instr)) ")")]


    [(New-struct? instr)
     (string-append "(" (New-struct-type instr) " " (reduce string-append (map (lambda (v) (string-append " " (to-string-instr v arg-store scope-num parent-scope))) (New-struct-arg-list  instr))) ")")]

    [(list? instr)
     (string-append "(list " (reduce (lambda (a b) (string-append a " " b)) (map (lambda (i) (to-string-instr i arg-store scope-num parent-scope)) instr)) ")")]


    [(None? instr)
     (string-append "(None)")]
    [(Is-none?? instr)
     (string-append "(None? " (to-string-instr (Is-none?-val instr) arg-store scope-num parent-scope) ")")]




    [(Mystery-const? instr)
     (string-append "??")]
                    

    [else
 
     (~v instr)]))



(define (sketch-unroll-repeat instr-list meta-var depth arg-store scope-num parent-scope)
  (cond
    [(equal? depth 0) (string-append "[(equal? " meta-var " 0) (begin (void))]\n")]
    [else
     (string-append
      "[(equal? " meta-var " " (~v depth) ") (begin \n" (instr-list-to-sketch (list-multiply instr-list depth) arg-store scope-num parent-scope) ")]\n"
      (sketch-unroll-repeat instr-list meta-var (- depth 1) arg-store))]))



(define (sketch-unroll-repeat-non-simul instr-list meta-var depth library arg-store ret-store scope-num parent-scope)
  (cond
    [(equal? depth 0) (string-append "[(equal? " meta-var " 0) (begin (void))]\n")]
    [else
     (string-append
      "[(equal? " meta-var " " (~v depth) ") (begin \n" (print-non-sketch-simulation (list-multiply instr-list depth) library arg-store ret-store scope-num parent-scope)  ;; (instr-list-to-sketch (list-multiply instr-list depth) arg-store)
      ")]\n"
      (sketch-unroll-repeat-non-simul instr-list meta-var (- depth 1) library arg-store ret-store scope-num parent-scope))]))
     
      





















;; Generates Sketch where we only separate traces that are actually different from each other
;; so if two traces have a common prefix, they only diverge when they have to
(define (trace-list-to-sketch trace-list library arg-store scope-num parent-scope)

  (define (equals-any-t-id t-list)
    ;; (displayln "equals-any-t-id")
    (cond
      [(empty? t-list) ""]
      [else
       (string-append "(equal? pick-trace " (~v (Trace-trace-id (first t-list))) ") "
                      (equals-any-t-id (rest t-list)))]))

  

  

  ;; Assumes all traces are non-empty
  (define (common-first-elem-trace-sets t-list)
    ;; (displayln "common-first-elem-trace-sets")
    (cond
      [(empty? t-list)
       `()]
      [else
       ;; (displayln "common ELSE case")

       (let ([begins-with-first
               (filter
                (lambda (t)
                  (command-equality-check (first (Trace-t (first t-list))) (first (Trace-t t))))
                   ;; (equal? (object-name (first (Trace-t (first t-list))))
                   ;;         (object-name (first (Trace-t t))))) ;; TODO need better equality check
                t-list)]

             [not-begins-with-first
              (filter
               (lambda (t)
                 (not (command-equality-check (first (Trace-t (first t-list))) (first (Trace-t t)))))
                 ;; (not (equal? (object-name (first (Trace-t (first t-list))))
                 ;;         (object-name (first (Trace-t t)))))) ;; TODO need better equality check


               t-list)])
                                                      

         (append
          (list begins-with-first)
          (common-first-elem-trace-sets not-begins-with-first)))]))

    (define (trace-list-to-sketch-recursive trace-list)
      (let
          ([split-traces (common-first-elem-trace-sets trace-list)])

        ;; (display "SPLIT-TRACES: ")
        ;; (displayln split-traces)
        (cond
          [(empty? split-traces) ;; (displayln "FOUND EMPTY")
           ""]
          [(equal? (length split-traces) 1)
           ;; (displayln "Recusive case - 1 split trace ")
           ;; (display "TRACE-FIRST: " ) (displayln (trace-list-to-sketch-recursive (rest-of-traces (first split-traces))))
           
           (define str (string-append
            (single-instr-to-sketch (first (Trace-t (first (first split-traces)))) library arg-store scope-num parent-scope)
            (trace-list-to-sketch-recursive (rest-of-traces (first split-traces)))))
           ;; (displayln "STRING FINISHED")
           str

             ]
          [(> (length split-traces) 1)
           ;; (displayln "Recursive case - NOT 1")
           (let ([max-length-split
                  (reduce
                   (lambda (l1 l2)
                     (if (> (length l1) (length  l2))
                         l1
                         l2))
                   split-traces)])
             ;; (display "max-length-split: ") (displayln max-length-split)
             ;; "")])))
             
             (let ([no-max-length-split-traces
                    (filter
                     (lambda (t-set)
                       (not (trace-ids-equal? t-set max-length-split)))
                     split-traces)])
               ;; (display "no-max-length-split: ") (displayln no-max-length-split-traces)
               ;; (display "FIRST MAX-LENGTH-SPLIT: ") (displayln (first (Trace-t (first max-length-split))))

               (define str (string-append
                "(cond \n"

                (reduce
                 string-append
                 (map
                  (lambda (t-set)
                    ;; (display "(first (trace-t (first t-set)))::::")
                    ;; (displayln (first (Trace-t (first t-set))))
                    (string-append
                     "[ (and POSSIBLE (or " (equals-any-t-id t-set) ")) " (single-instr-to-sketch (first (Trace-t (first t-set))) library arg-store scope-num parent-scope) (trace-list-to-sketch-recursive (rest-of-traces t-set)) "]\n"))
                  no-max-length-split-traces))

                "[POSSIBLE \n"
                (single-instr-to-sketch (first (Trace-t (first max-length-split))) library arg-store scope-num parent-scope)
                (trace-list-to-sketch-recursive (rest-of-traces max-length-split))"])\n"
                ))

               ;; (displayln "NOT 1 STRING FINISHED")

               str


               ))])))

             
                 
              
              
             



  (let
      ([split-traces (common-first-elem-trace-sets trace-list)])

    (cond
      [(> (length split-traces) 1)
       ;; (displayln "LENGTH > 1")
       (string-append
        "(cond \n"
       (reduce
        string-append
        (map
         (lambda (t-set)
           (string-append
            "[ (or " (equals-any-t-id t-set) ") " (single-instr-to-sketch (first (Trace-t (first t-set))) library arg-store scope-num parent-scope) (trace-list-to-sketch-recursive (rest-of-traces t-set)) "]\n"))
         split-traces))

       
       ")\n")]
      [(equal? (length split-traces) 1)
       ;; (displayln "LENGTH = 1")
       ;; (displayln (Trace-t (first (first split-traces))))
       (single-instr-to-sketch (first (Trace-t (first (first split-traces)))) library arg-store scope-num parent-scope)]
      [else
       ""])))



;; Translates library methods into racket code
(define (generate-library-code library)
  (reduce
   string-append

   (map
    (lambda (method)
      (string-append
       "\n\n(define (METHOD-" (Method-id method) " arg-list)"
       "(define TO-RETURN (void))\n"
       (all-instrs-to-sketch (retrieve-code library (Method-id method)) library)
       "(set! method-exit #f)\n"
       "TO-RETURN"
       ")\n\n\n\n"))

    library)))

;; Combines the single-instr-to-sketch for all the instructions in a list of instructions
(define (all-instrs-to-sketch instr-list library)
  (print-non-sketch-simulation instr-list library "arg-list" "" 0 0))

  
  


;; Translates individual instruction to sketch instruction
(define (single-instr-to-sketch instr library arg-store scope-num parent-scope)
  ;; (display "to-sketch: ")(display (first instr-list))(display "\n")
  (cond
         [(Create-var? instr)
          ;; (display "create-var-id: ") (display (Create-var-id instr)) (display "\n")
          ;; (new-binding (Create-var-id instr) scope-num parent-scope)
          (string-append
           "(define " (Create-var-id instr) ;; (~v (get-most-recent-binding (Create-var-id instr) scope-num parent-scope))
           " (void))\n")]



         [(Lock? instr)
          (string-append "(if (not (has-lock current-thread " (~v (Lock-id instr)) " ))\n"
                         "(set! POSSIBLE #f)"
                         "(begin (void))) \n")]
                         
                         




         [(Run-method? instr) 
          (let ([args-list (new-arg-list-id)]
                [new-scope (new-scope-num)])
            ;; (add-binding-parent new-scope scope-num)
            (string-append
             "(displayln \"running method: " (Run-method-method instr) "\")"
             "(set! current-thread " (~v (C-Instruction-thread-id instr)) ")\n"
             ;; "(set! method-exit #f)\n"
             "(METHOD-" (Run-method-method instr) " " (to-string-instr (Run-method-args instr) arg-store scope-num parent-scope) ")\n"
             ;; "(define " args-list " (void))\n"
             ;; "(set! " args-list " " (to-string-instr (Run-method-args instr) arg-store scope-num parent-scope) ")\n"
             ;; "(begin\n"
             ;; (print-non-sketch-simulation (retrieve-code library (Run-method-method instr)) library args-list (Run-method-ret instr) new-scope scope-num)
             ;; ")\n"
             ;; (print-non-sketch-simulation (rest instr-list) library args-list ret-store)
             ))]

         [(Set-var? instr)
          ;; "todo\n"]
          ;; (display "found set var\n")
          (string-append
           "(set! " (Set-var-id instr) ;; (~v (get-most-recent-binding (Set-var-id instr) scope-num parent-scope))
           " " (to-string-instr (Set-var-assignment instr)  arg-store scope-num parent-scope) ")\n")]




         [(Assume-simulation? instr)

          (string-append
           "(if (not " (to-string-instr (Assume-simulation-condition instr)  arg-store scope-num parent-scope) ")\n (set! POSSIBLE #f)\n (begin (void)))\n")]


         [(Assume-loop? instr)
          (string-append
           "(if (not " (to-string-instr (Assume-loop-condition instr) arg-store scope-num parent-scope) ")\n (set! POSSIBLE #f)\n (begin \n"
           "(void)))\n")]


         [(Set-pointer? instr)
          (string-append
           "(set-" (Set-pointer-type instr) "-" (Set-pointer-offset instr) "! " (Set-pointer-id instr)  ;; (~v (get-most-recent-binding (Set-pointer-id instr) scope-num parent-scope))
           " "  (to-string-instr (Set-pointer-val instr)  arg-store scope-num parent-scope) ")\n")]


         [(Assume-meta? instr)
          (string-append
           "(if (not meta-var" (~v (Assume-meta-condition instr)) ")\n (set! POSSIBLE #f) (begin  \n"
           "(void)))\n")]

         [(Assume-not-meta? instr)
          (string-append
           "(if meta-var" (~v (Assume-not-meta-condition instr)) "\n (set! POSSIBLE #f)\n (begin\n"
           "(void)))\n")]

         [(Repeat-meta? instr)
          (string-append
           "(cond "
           (sketch-unroll-repeat (Repeat-meta-instr-list instr) "meta-count" 3 library arg-store scope-num parent-scope)
           ")"
           
           )]

         ;; [(Meta-branch? instr)
         ;;  (string-append
         ;;   "(if meta-var" (~v (Meta-branch-condition instr)) "(begin\n"
         ;;   (instr-list-to-sketch (Meta-branch-branch1 instr) library arg-store scope-num parent-scope)
         ;;   ")\n"
         ;;   "(begin\n" (instr-list-to-sketch (Meta-branch-branch2 instr) library arg-store scope-num parent-scope) ")\n")]

         ;; [(Maybe-loop? instr)
         ;;  (string-append
         ;;   "(if meta-var" (~v (Maybe-loop-meta-var instr)) "(begin\n"
         ;;   (maybe-loop-to-sketch (Maybe-loop-condition instr)
         ;;                         (Maybe-loop-instr-list1 instr)
         ;;                         (Maybe-loop-instr-list2 instr)
         ;;                         (Maybe-loop-hole instr)
         ;;                         library arg-store scope-num parent-scope)
         ;;   ")\n (begin\n"
         ;;   (instr-list-to-sketch (Maybe-loop-original-instr-list instr) library arg-store scope-num parent-scope) "))\n")]


         [(CAS? instr)
          (string-append

           "(if  (not (equal? " (to-string-instr (CAS-v1 instr)  arg-store scope-num parent-scope) " " (to-string-instr (CAS-v2 instr) arg-store scope-num parent-scope) "))\n"
           "(begin\n "
           "(set! " (to-string-instr (CAS-v1 instr)  arg-store scope-num parent-scope) " " (to-string-instr (CAS-new-val instr) arg-store scope-num parent-scope) ")\n"
           "(set! " (CAS-ret instr) ;; (~v (get-most-recent-binding (CAS-ret instr) scope-num parent-scope))
           " 1))"
           "(begin\n "
           "(set! " (CAS-ret instr) ;; (~v (get-most-recent-binding (CAS-ret instr) scope-num parent-scope))
           " 0)))\n"

           )]
           

         [(Continue? instr)
          (string-append "TODO: continue\n")]
          ;; (let ([where-to (find-continue-sublist (rest instr-list) (Continue-to-where instr))])
          ;;   ;; (display (Continue-to-where instr)) (display "\n")
          ;;   (instr-list-to-sketch where-to library arg-store scope-num parent-scope))]



         [else
          ;; (display "todo case\n")
          (string-append
           ;; "TODO\n"
           ";;TODO-single-instr:"
           (~v instr) "\n")]))




(define (instr-list-to-sketch instr-list library arg-store scope-num parent-scope)
  ;; (display "to-sketch: ")(display (first instr-list))(display "\n")
  (cond
    [(empty? instr-list) ""]
    [else
     (let ([elem (first instr-list)])
       (cond
         [(Create-var? elem)
          ;; (display "create-var-id: ") (display (Create-var-id elem)) (display "\n")
          ;; (new-binding (Create-var-id elem) scope-num parent-scope)
          (string-append
           "(define " (Create-var-id elem) ;; (~v (get-most-recent-binding (Create-var-id elem) scope-num parent-scope))
           " (void))\n"
           (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope))]


         [(Lock? elem)
          (string-append "(if (not (has-lock current-thread " (~v (Lock-id elem)) " ))\n"
                         "(set! POSSIBLE #f)"
                         "(begin \n"
                         (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope)
                         "))")]




         [(Run-method? elem) 
          (let ([args-list (new-arg-list-id)]
                [new-scope (new-scope-num)])
            ;; (add-binding-parent new-scope scope-num)
            (string-append
             "(displayln \"running method: " (Run-method-method elem) "\")"
             "(set! current-thread " (~v (C-Instruction-thread-id elem)) ")\n"
             "(set! method-exit #f)\n"

             "(display \"result: \")"

             "(set! TMP-RET (METHOD-" (Run-method-method elem) " " (to-string-instr (Run-method-args elem) arg-store scope-num parent-scope) "))\n"
             "(displayln TMP-RET)\n"
             "(set! " (format "~a" (Run-method-ret elem)) " TMP-RET)\n"
             ;; "(define " args-list " (void))\n"
             ;; "(set! " args-list " " (to-string-instr (Run-method-args elem) arg-store scope-num parent-scope) ")\n"
             ;; "(begin\n"
             ;; (print-non-sketch-simulation (retrieve-code library (Run-method-method elem)) library args-list (Run-method-ret elem) new-scope scope-num)
             ;; ")\n"
             (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope)
             ;; (print-non-sketch-simulation (rest instr-list) library args-list ret-store)
             ))]

         [(Set-var? elem)
          ;; "todo\n"]
          ;; (display "found set var\n")
          (string-append
           "(set! " (Set-var-id elem) ;; (~v (get-most-recent-binding (Set-var-id elem) scope-num parent-scope))
           " " (to-string-instr (Set-var-assignment elem)  arg-store scope-num parent-scope) ")\n"
           (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope))]

         [(Assume-simulation? elem)
          ;; (display "printing out assume simulation: ") (displayln elem)
          (string-append
           "(if (not " (to-string-instr (Assume-simulation-condition elem)  arg-store scope-num parent-scope) ")\n (set! POSSIBLE #f)\n (begin \n"
           (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope) ""
           "))\n")]

         [(Assume-loop? elem)
          (string-append
           "(if (not " (to-string-instr (Assume-loop-condition elem) arg-store scope-num parent-scope) ")\n (set! POSSIBLE #f)\n (begin \n"
           (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope) ""
           "))\n")]


         [(Set-pointer? elem)
          (string-append
           "(set-" (Set-pointer-type elem) "-" (Set-pointer-offset elem) "! " (Set-pointer-id elem)  ;; (~v (get-most-recent-binding (Set-pointer-id elem) scope-num parent-scope))
           " "  (to-string-instr (Set-pointer-val elem)  arg-store scope-num parent-scope) ")\n"
           (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope))]

         [(Assume-meta? elem)
          (string-append
           "(if (not meta-var" (~v (Assume-meta-condition elem)) ")\n (set! POSSIBLE #f) (begin  \n"
           (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope) "))\n")]

         [(Assume-not-meta? elem)
          (string-append
           "(if meta-var" (~v (Assume-not-meta-condition elem)) "\n (set! POSSIBLE #f)\n (begin\n"
           (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope) "))\n")]

         [(Repeat-meta? elem)
          (string-append
           "(cond "
           (sketch-unroll-repeat (Repeat-meta-instr-list elem) "meta-count" 3 library arg-store scope-num parent-scope)
           ")"
           
           (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope))]

         ;; [(Meta-branch? elem)
         ;;  (string-append
         ;;   "(if meta-var" (~v (Meta-branch-condition elem)) "(begin\n"
         ;;   (instr-list-to-sketch (Meta-branch-branch1 elem) library arg-store scope-num parent-scope)
         ;;   ")\n"
         ;;   "(begin\n" (instr-list-to-sketch (Meta-branch-branch2 elem) library arg-store scope-num parent-scope) ")\n")]

         ;; [(Maybe-loop? elem)
         ;;  (string-append
         ;;   "(if meta-var" (~v (Maybe-loop-meta-var elem)) "(begin\n"
         ;;   (maybe-loop-to-sketch (Maybe-loop-condition elem)
         ;;                         (Maybe-loop-instr-list1 elem)
         ;;                         (Maybe-loop-instr-list2 elem)
         ;;                         (Maybe-loop-hole elem)
         ;;                         library arg-store scope-num parent-scope)
         ;;   ")\n (begin\n"
         ;;   (instr-list-to-sketch (Maybe-loop-original-instr-list elem) library arg-store scope-num parent-scope) "))\n")]


         [(CAS? elem)
          (string-append

           "(if  (not (equal? " (to-string-instr (CAS-v1 elem)  arg-store scope-num parent-scope) " " (to-string-instr (CAS-v2 elem) arg-store scope-num parent-scope) "))\n"
           "(begin\n "
           "(set! " (to-string-instr (CAS-v1 elem)  arg-store scope-num parent-scope) " " (to-string-instr (CAS-new-val elem) arg-store scope-num parent-scope) ")\n"
           "(set! " (CAS-ret elem) ;; (~v (get-most-recent-binding (CAS-ret elem) scope-num parent-scope))
           " 1))"
           "(begin\n "
           "(set! " (CAS-ret elem) ;; (~v (get-most-recent-binding (CAS-ret elem) scope-num parent-scope))
           " 0)))\n"

           (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope))]
           

         [(Continue? elem)
          (let ([where-to (find-continue-sublist (rest instr-list) (Continue-to-where elem))])
            ;; (display (Continue-to-where elem)) (display "\n")
            (instr-list-to-sketch where-to library arg-store scope-num parent-scope))]

         [(Return? elem)
          (string-append "(set! RETURN-VAL " (to-string-instr (Return-val elem) arg-store scope-num parent-scope) ")\n"
                         (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope))]

         [(Trace-Type? elem)
          (string-append "(set! TRACE-TYPE " (to-string-instr (Trace-Type-tp elem) arg-store scope-num parent-scope) ")\n"
                         (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope))]


         [(Optimistic-Check? elem)
          (cond
            [(Optimistic-Check-which-val elem)
             (string-append "(set! OPT" (~v (Optimistic-Check-opt-id elem)) "-true-list (append (list (OPT" (~v (Optimistic-Check-opt-id elem)) "))  OPT" (~v (Optimistic-Check-opt-id elem)) "-true-list))\n"
                            (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope))]
            [else
             (string-append "(set! OPT" (~v (Optimistic-Check-opt-id elem)) "-false-list (append (list (OPT" (~v (Optimistic-Check-opt-id elem)) "))  OPT" (~v (Optimistic-Check-opt-id elem)) "-false-list))\n"
                            (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope))])]


         [else
          ;; (display "todo case\n")
          (string-append
           ;; "TODO\n"
           ";;TODO-instr-list-to-sketch:"
           (~v (first instr-list)) "\n"
           (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope))]))]))
           


         
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



(define (generate-optimistic-condition-lists opt-conds)
  (reduce
   string-append
   (map (lambda (c) (string-append "(define OPT" (~v (Optimistic-Check-opt-id c)) "-true-list (void))\n"
                                   "(set! OPT" (~v (Optimistic-Check-opt-id c)) "-true-list `())\n"
                                   "(define OPT" (~v (Optimistic-Check-opt-id c)) "-false-list (void))\n"
                                   "(set! OPT" (~v (Optimistic-Check-opt-id c)) "-false-list `())\n"))
        opt-conds)))
   
   


;; Optimistic Concurrency condition grammar generated for each Optimistic-Condition
;; object given.
(define (generate-optimistic-condition-sketches opt-conds depth)
  (reduce
   string-append
   (map
    (lambda (opt-cond)

      (let
          ([opt-meta-id
            (cond
              [(Optimistic-Condition? opt-cond) (~v (Optimistic-Condition-meta-var opt-cond))]
              [(Optimistic-Check? opt-cond) (~v (Optimistic-Check-opt-id opt-cond))]
              [else
               (displayln "opt-meta-id not found...")
               (exit)])])
        (string-append
         "

  (define-synthax (optimistic-condition" opt-meta-id "  depth)
    #:base (choose (lambda ()  (METHOD-contains (list (list-ref first-args 0) (list-ref first-args 1))))
                    (lambda () (not (METHOD-contains (list (list-ref first-args 0) (list-ref first-args 1))))))

    #:else (choose
              (lambda () (METHOD-contains (list (list-ref first-args 0) (list-ref first-args 1))))
              (lambda () (not (METHOD-contains (list (list-ref first-args 0) (list-ref first-args 1)))))
              (lambda () ((choose && ||) ((optimistic-condition" opt-meta-id " (- depth 1)))
                              ((optimistic-condition" opt-meta-id " (- depth 1)))))))

 (define OPT1"

                              " (optimistic-condition" opt-meta-id " 2))")))






    
 ;; (~v (Optimistic-Condition-meta-var opt-cond)) " (lambda () (optimistic-condition" (~v (Optimistic-Condition-meta-var opt-cond)) " 1)))\n"))

    

opt-conds)))




;; Top level grammar to connect all the parts
(define (generate-top-level-grammar opt-info-list arg-store depth)

  (define (range1 i j)
    (cond
      [(> i j) `()]
      [(equal? i j) (list i)]
      [else
       (append (list i) (range1 (+ i 1) j))]))
  
  (reduce
   string-append
   (map
    (lambda (opt-info)
      (let ([opt-meta-id (~v (Optimistic-Info-id opt-info))]
            [opt-possible-vals (Optimistic-Info-possible-vals opt-info)])
        (string-append
         "(define OPT" opt-meta-id " (lambda ()
(choose
 (not (equal? (method-choice" opt-meta-id (~v depth)") (var-choice" opt-meta-id (~v depth)")))\n"


(cond
  [(not (equal? depth 1))
   (reduce
    string-append
    (map
     (lambda (i)
       (string-append
        "((choose || &&) "
        (reduce
         string-append
         (map
          (lambda (j)
            (string-append "(not (equal? (method-choice" opt-meta-id (~v j)") (var-choice" opt-meta-id (~v j) "))) "))
          (range1 1 i)))
        ")\n"))
     (range1 2 depth)))
   ")))\n"]

  [else
   ")))\n"]))))

opt-info-list)))

             


;; Generates optimistic condition grammar based on trace consistency checks
(define (generate-smart-optimistic-condition-grammar opt-info-list arg-store depth)
  (cond
    [(equal? depth 0) ""]
    [else

     (string-append

      
     (reduce
      string-append
      (map
       (lambda (opt-info)
         (let ([opt-meta-id (~v (Optimistic-Info-id opt-info))]
               [opt-possible-vals (Optimistic-Info-possible-vals opt-info)])

           (string-append
            "
(define method-choice" opt-meta-id (~v depth) "\n"
 "            (choose\n"

               (reduce
string-append
(map
 (lambda (elem)
   (string-append "(lambda () (METHOD-" (Run-method-method elem) " " (to-string-instr (Run-method-args elem) arg-store 0 `()) "))\n"))
 opt-possible-vals))

"))\n"

)))



 
opt-info-list))

     (reduce
      string-append
      (map
       (lambda (opt-info)
         (let ([opt-meta-id (~v (Optimistic-Info-id opt-info))]
               [opt-possible-vals (Optimistic-Info-possible-vals opt-info)])

           (string-append
            "
(define var-choice" opt-meta-id (~v depth) "\n"
 "            (choose\n"

               (reduce
string-append
(map
 (lambda (elem)
   (string-append "(lambda () "(Run-method-ret elem) ")\n"))
 opt-possible-vals))

"))\n"

)))

opt-info-list))


;; (reduce
;;  string-append
;;  (map
;;   (lambda (opt-info)
;;     (let ([opt-meta-id (~v (Optimistic-Info-id opt-info))]
;;           [opt-possible-vals (Optimistic-Info-possible-vals opt-info)])
      
;;           (string-append
;;            "
;; (define grammar" opt-meta-id (~v depth) "\n"
;;  "            (choose\n
;;                (lambda () (not (equal? (method-choice" opt-meta-id (~v depth)") (var-choice" opt-meta-id (~v depth)"))))"
;; (if (not (equal? depth 1))
;;     (string-append "(lambda () ((choose || &&) (grammar" opt-meta-id (~v (- depth 1))") (grammar" opt-meta-id (~v (- depth 1))")))))")
;;     "))\n")





;; )))



 
;; opt-info-list))



(generate-smart-optimistic-condition-grammar opt-info-list arg-store (- depth 1)))]))









        

      
     
     

;; (define (generate-smart-optimistic-condition-grammar opt-info-list arg-store depth)
;;   (reduce
;;    string-append
;;    (map
;;     (lambda (opt-info)

;;       (let
;;           ([opt-meta-id (~v (Optimistic-Info-id opt-info))]
;;            [opt-possible-vals (Optimistic-Info-possible-vals opt-info)])

;;         (string-append "

;; (define-synthax (optimistic-condition" opt-meta-id " depth)\n"
;; "#:base (choose "
;;   (reduce
;; string-append

;; (map
;;  (lambda (elem)
;;    (string-append 
;;     " (lambda () (equal? " (Run-method-ret elem) " (METHOD-" (Run-method-method elem) " " (to-string-instr (Run-method-args elem) arg-store 0 `()) ")))\n"))
;;  opt-possible-vals)


;; )
;; ")\n"
;; "#:else (choose "


;; (reduce
;;  string-append
;;  (map
;;   (lambda (elem)
;;     (string-append
;;      " (lambda () (equal? " (Run-method-ret elem) " (METHOD-" (Run-method-method elem) " " (to-string-instr (Run-method-args elem) arg-store 0 `()) ")))\n"))
;;   opt-possible-vals))


;; "( (choose || &&) ((optimistic-condition" opt-meta-id " (- depth 1))) ((optimistic-condition" opt-meta-id " (- depth 1))) )))\n"


;;  "(define OPT" opt-meta-id

;;                               " (optimistic-condition" opt-meta-id " 2))"


;; )))

;; opt-info-list)))

   
            
  
