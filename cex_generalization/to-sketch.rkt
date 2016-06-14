#lang racket 
(require "../program_representation/simulator-structures.rkt")
(require racket/string)

(provide
 to-string-instr
 instr-list-to-sketch)

(define (to-string-instr instr)
  ;; (display "to-string: ") (display instr) (display "\n")
  (cond
    [(Dereference? instr)
     (string-append
      "("(Dereference-type instr) "-" (Dereference-offset instr) " " (Dereference-id instr) ")")]
    [(Equal? instr)
     (string-append "(equal? " (to-string-instr (Equal-expr1 instr)) " " (to-string-instr (Equal-expr2 instr)) ")")]
    [(Or? instr)
     (string-append "(or " (to-string-instr (Or-expr1 instr)) " " (to-string-instr (Or-expr2 instr)) ")")]
    [(Not? instr)
     (string-append "(not " (to-string-instr (Not-expr instr)) ")")]
    [(And? instr)
     (string-append "(and " (to-string-instr (And-expr1 instr)) " " (to-string-instr (And-expr2 instr)) ")")]
    [(Get-var? instr)
     (string-append (Get-var-id instr))]




    [(Mystery-const? instr)
     (string-append "??")]
                    

    [else

     (~v instr)]))



(define (list-multiply instr-list num)
  (cond
    [(equal? num 0) `()]
    [else
     (append instr-list (list-multiply instr-list (- num 1)))]))

(define (sketch-unroll-repeat instr-list meta-var depth)
  (cond
    [(equal? depth 0) (string-append "[(equal? " meta-var " 0) (begin)]\n")]
    [else
     (string-append
      "[(equal? " meta-var " " (~v depth) ") (begin \n" (instr-list-to-sketch (list-multiply instr-list depth)) ")]\n"
      (sketch-unroll-repeat instr-list meta-var (- depth 1)))]))
     
      

(define (instr-list-to-sketch instr-list)
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
           (instr-list-to-sketch (rest instr-list)))]


         [(Lock? elem)
          (string-append "(if (not (has-lock current-thread " (~v (Lock-id elem)) " ))\n"
                         "#f"
                         "(begin \n"
                         (instr-list-to-sketch (rest instr-list))
                         "))")]


         [(Set-var? elem)
          ;; "todo\n"]
          ;; (display "found set var\n")
          (string-append
           "(set! " (Set-var-id elem) " " (to-string-instr (Set-var-assignment elem)) ")\n"
           (instr-list-to-sketch (rest instr-list)))]

         [(Assume-simulation? elem)
          (string-append
           "(if " (to-string-instr (Assume-simulation-condition elem)) "\n #f\n (begin \n"
           (instr-list-to-sketch (rest instr-list)) ""
           "))\n")]

         [(Assume-loop? elem)
          (string-append
           "(if " (to-string-instr (Assume-loop-condition elem)) "\n #f\n (begin \n"
           (instr-list-to-sketch (rest instr-list)) ""
           "))\n")]


         [(Set-pointer? elem)
          (string-append
           "(set-" (Set-pointer-type elem) "-" (Set-pointer-offset elem) "! " (Set-pointer-id elem) " "  (to-string-instr (Set-pointer-val elem)) ")\n"
           (instr-list-to-sketch (rest instr-list)))]

         [(Assume-meta? elem)
          (string-append
           "(if (not meta-var)\n #f (begin  \n"
           (instr-list-to-sketch (rest instr-list)) "))\n")]

         [(Repeat-meta? elem)
          (string-append
           "(cond "
           (sketch-unroll-repeat (Repeat-meta-instr-list elem) "meta-count" 3)
           ")"
           
           (instr-list-to-sketch (rest instr-list)))]


         [(CAS? elem)
          (string-append

           "(if (not (equal? " (to-string-instr (CAS-v1 elem)) " " (to-string-instr (CAS-v2 elem)) "))\n"
           "(begin\n "
           "(set! " (to-string-instr (CAS-v1 elem)) " " (to-string-instr (CAS-new-val elem)) ")\n"
           "(set! " (CAS-ret elem) " 1))"
           "(begin\n "
           "(set! " (CAS-ret elem) " 0)))\n"

           (instr-list-to-sketch (rest instr-list)))]
           

         [(Continue? elem)
          (let ([where-to (find-continue-sublist (rest instr-list) (Continue-to-where elem))])
            ;; (display (Continue-to-where elem)) (display "\n")
            (instr-list-to-sketch where-to))]



         [else
          ;; (display "todo case\n")
          (string-append
           ;; "TODO\n"
           ";;TODO:"
           (~v (first instr-list)) "\n"
           (instr-list-to-sketch (rest instr-list)))]))]))
           


         
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
