#lang racket 
(require "../program_representation/simulator-structures.rkt")

(require racket/string)
(require racket/hash)

(provide
 to-string-instr
 print-non-sketch-simulation
 add-binding-parent
 retrieve-code
 new-scope-num
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
    

(define (print-non-sketch-simulation instr-list library arg-store ret-store scope-num parent-scope)
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
          (let ([args-list (new-arg-list-id)]
                [new-scope (new-scope-num)])
            (add-binding-parent new-scope scope-num)
            (string-append
             "(set! current-thread " (~v (C-Instruction-thread-id elem)) ")\n"
             "(set! method-exit #f)\n"
             "(define " args-list " (void))\n"
             "(set! " args-list " " (to-string-instr (Run-method-args elem) arg-store scope-num parent-scope) ")\n"
             "(begin\n"
             (print-non-sketch-simulation (retrieve-code library (Run-method-method elem)) library args-list (Run-method-ret elem) new-scope scope-num)
             ")\n"
             (print-non-sketch-simulation (rest instr-list) library args-list ret-store scope-num parent-scope)
             ))]
           
          
          

         
         [(Create-var? elem)
          ;; (display "create-var-id: ") (display (Create-var-id elem)) (display "\n")
          (new-binding (Create-var-id elem) scope-num parent-scope)
          (string-append
           ;; "(display \"creating.... " (Create-var-id elem) "\n\")"
           "(define " (Create-var-id elem) (~v (get-most-recent-binding (Create-var-id elem)  scope-num parent-scope)) " (void))\n"
           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store scope-num parent-scope))]


         [(Lock? elem)
          (string-append "(if (not (has-lock current-thread " (~v (Lock-id elem)) " ))\n"
                         "#f\n"
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
           "(set! " (Set-var-id elem) (~v (get-most-recent-binding (Set-var-id elem) scope-num parent-scope)) " " (to-string-instr (Set-var-assignment elem) arg-store scope-num parent-scope) ")\n"
           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store scope-num parent-scope))]



         [(Set-pointer? elem)
          (string-append
           "(set-" (Set-pointer-type elem) "-" (Set-pointer-offset elem) "! " (Set-pointer-id elem)  (~v (get-most-recent-binding (Set-pointer-id elem) scope-num parent-scope)) " "  (to-string-instr (Set-pointer-val elem) arg-store scope-num parent-scope) ")\n"
           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store scope-num parent-scope))]


         [(CAS? elem)
          (string-append

           "(if (not (equal? " (to-string-instr (CAS-v1 elem) arg-store scope-num parent-scope) " " (to-string-instr (CAS-v2 elem) arg-store scope-num parent-scope) "))\n"
           "(begin\n "
           "(" (special-set-string-instr (CAS-v1 elem) arg-store scope-num parent-scope) " " (to-string-instr (CAS-new-val elem) arg-store scope-num parent-scope) ")\n"
           ;; "(set! " (special-set-string-instr (CAS-v1 elem) arg-store) " " (to-string-instr (CAS-new-val elem) arg-store) ")\n"
           "(set! " (CAS-ret elem) (~v (get-most-recent-binding (CAS-ret elem) scope-num parent-scope)) " 1))"
           "(begin\n "
           "(set! " (CAS-ret elem) (~v (get-most-recent-binding (CAS-ret elem) scope-num parent-scope)) " 0)))\n"

           (print-non-sketch-simulation (rest instr-list) library arg-store ret-store scope-num parent-scope))]
           

         [(Continue? elem)
          (let ([where-to (find-continue-sublist (rest instr-list) (Continue-to-where elem))])
            ;; (display (Continue-to-where elem)) (display "\n")

            "(void)")]
             ;; (print-non-sketch-simulation `() library arg-store ret-store))]


         [(Return? elem)
          (string-append
           ;; "(display \"returning....."  (to-string-instr (Return-val elem) arg-store scope-num parent-scope) "\n\")\n"
           "(set! " ret-store (~v (get-most-recent-binding ret-store scope-num parent-scope)) " " (to-string-instr (Return-val elem) arg-store scope-num parent-scope) ")\n"
           "(set! method-exit #t)\n"
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
    [(Dereference? instr)
     (cond
       [(Dereference? (Dereference-id instr))
        (string-append
         "("(Dereference-type instr) "-" (Dereference-offset instr) " " (to-string-instr (Dereference-id instr) arg-store scope-num parent-scope) ")")]

        ;; (string-append
        ;;  "("(Dereference-type instr) "-" (Dereference-offset instr) " " (to-string-instr (Get-var (to-string-instr (Dereference-id instr) arg-store scope-num parent-scope)) arg-store scope-num parent-scope) ")")]
        [else

         (string-append
          "("(Dereference-type instr) "-" (Dereference-offset instr) " "  (Dereference-id instr) (~v (get-most-recent-binding (Dereference-id instr) scope-num parent-scope)) ")")])]
    [(Equal? instr)
     (string-append "(equal? " (to-string-instr (Equal-expr1 instr) arg-store scope-num parent-scope) " " (to-string-instr (Equal-expr2 instr) arg-store scope-num parent-scope) ")")]
    [(Or? instr)
     (string-append "(or " (to-string-instr (Or-expr1 instr) arg-store scope-num parent-scope) " " (to-string-instr (Or-expr2 instr) arg-store scope-num parent-scope) ")")]
    [(Not? instr)
     (string-append "(not " (to-string-instr (Not-expr instr) arg-store scope-num parent-scope) ")")]
    [(And? instr)
     (string-append "(and " (to-string-instr (And-expr1 instr) arg-store scope-num parent-scope) " " (to-string-instr (And-expr2 instr) arg-store scope-num parent-scope) ")")]
    [(Get-var? instr)
     (string-append (Get-var-id instr) (~v (get-most-recent-binding (Get-var-id instr) scope-num parent-scope)))]
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



(define (list-multiply instr-list num)
  (cond
    [(equal? num 0) `()]
    [else
     (append instr-list (list-multiply instr-list (- num 1)))]))

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
     
      



  



(define (instr-list-to-sketch instr-list library arg-store scope-num parent-scope)
  ;; (display "to-sketch: ")(display (first instr-list))(display "\n")
  (cond
    [(empty? instr-list) ""]
    [else
     (let ([elem (first instr-list)])
       (cond
         [(Create-var? elem)
          ;; (display "create-var-id: ") (display (Create-var-id elem)) (display "\n")
          (new-binding (Create-var-id elem) scope-num parent-scope)
          (string-append
           "(define " (Create-var-id elem) (~v (get-most-recent-binding (Create-var-id elem) scope-num parent-scope)) " (void))\n"
           (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope))]


         [(Lock? elem)
          (string-append "(if (not (has-lock current-thread " (~v (Lock-id elem)) " ))\n"
                         "#f"
                         "(begin \n"
                         (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope)
                         "))")]




         [(Run-method? elem)
          (let ([args-list (new-arg-list-id)]
                [new-scope (new-scope-num)])
            (add-binding-parent new-scope scope-num)
            (string-append
             "(set! current-thread " (~v (C-Instruction-thread-id elem)) ")\n"
             "(set! method-exit #f)\n"
             "(define " args-list " (void))\n"
             "(set! " args-list " " (to-string-instr (Run-method-args elem) arg-store scope-num parent-scope) ")\n"
             "(begin\n"
             (print-non-sketch-simulation (retrieve-code library (Run-method-method elem)) library args-list (Run-method-ret elem) new-scope scope-num)
             ")\n"
             (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope)
             ;; (print-non-sketch-simulation (rest instr-list) library args-list ret-store)
             ))]

         [(Set-var? elem)
          ;; "todo\n"]
          ;; (display "found set var\n")
          (string-append
           "(set! " (Set-var-id elem) (~v (get-most-recent-binding (Set-var-id elem) scope-num parent-scope)) " " (to-string-instr (Set-var-assignment elem)  arg-store scope-num parent-scope) ")\n"
           (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope))]

         [(Assume-simulation? elem)
          (string-append
           "(if (not " (to-string-instr (Assume-simulation-condition elem)  arg-store scope-num parent-scope) ")\n #f\n (begin \n"
           (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope) ""
           "))\n")]

         [(Assume-loop? elem)
          (string-append
           "(if (not " (to-string-instr (Assume-loop-condition elem) arg-store scope-num parent-scope) ")\n #f\n (begin \n"
           (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope) ""
           "))\n")]


         [(Set-pointer? elem)
          (string-append
           "(set-" (Set-pointer-type elem) "-" (Set-pointer-offset elem) "! " (Set-pointer-id elem)  (~v (get-most-recent-binding (Set-pointer-id elem) scope-num parent-scope)) " "  (to-string-instr (Set-pointer-val elem)  arg-store scope-num parent-scope) ")\n"
           (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope))]

         [(Assume-meta? elem)
          (string-append
           "(if (not meta-var)\n #f (begin  \n"
           (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope) "))\n")]

         [(Repeat-meta? elem)
          (string-append
           "(cond "
           (sketch-unroll-repeat (Repeat-meta-instr-list elem) "meta-count" 3 library arg-store scope-num parent-scope)
           ")"
           
           (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope))]


         [(CAS? elem)
          (string-append

           "(if  (not (equal? " (to-string-instr (CAS-v1 elem)  arg-store scope-num parent-scope) " " (to-string-instr (CAS-v2 elem) arg-store scope-num parent-scope) "))\n"
           "(begin\n "
           "(set! " (to-string-instr (CAS-v1 elem)  arg-store scope-num parent-scope) " " (to-string-instr (CAS-new-val elem) arg-store scope-num parent-scope) ")\n"
           "(set! " (CAS-ret elem) (~v (get-most-recent-binding (CAS-ret elem) scope-num parent-scope)) " 1))"
           "(begin\n "
           "(set! " (CAS-ret elem) (~v (get-most-recent-binding (CAS-ret elem) scope-num parent-scope)) " 0)))\n"

           (instr-list-to-sketch (rest instr-list) library arg-store scope-num parent-scope))]
           

         [(Continue? elem)
          (let ([where-to (find-continue-sublist (rest instr-list) (Continue-to-where elem))])
            ;; (display (Continue-to-where elem)) (display "\n")
            (instr-list-to-sketch where-to library arg-store scope-num parent-scope))]



         [else
          ;; (display "todo case\n")
          (string-append
           ;; "TODO\n"
           ";;TODO:"
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
