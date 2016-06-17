#lang racket

(require "../program_representation/simulator-structures.rkt")
(require racket/string)
(require "to-sketch.rkt")

(provide interleaving-to-sketch create-announcement-version)
 
(define counter (void))
(set! counter 0)
(define (freshvar)
  (set! counter (+ counter 1))
  counter)


(define (append-list l)
  (lambda (l2)
    (append l l2)))


(define (append-item i)
  (lambda (l2)
    (append (list i) l2)))




(define (all-combinations l1-set l2-set)

  (cond
    [(empty? l1-set) (list )]
    [(empty? l2-set) (list)]
    [else
     (append
      (map (append-list (first l1-set)) l2-set)
      (all-combinations (rest l1-set) l2-set))]))





(define (unroll-loop loop depth loop-id)
  ;; (display "unrolling at depth...") (display depth) (display "\n")
  (define (not-empty l)
    (not (empty? l)))
  (cond
    [(equal? depth 0)  (list (list (Assume-loop (lambda (e)  ;; (display (not ((Loop-condition loop) e))) (display "\n")
                                                  (not ((Loop-condition loop) e))) loop-id)))]
    [else
     (let ([possible-executions (possible-single-loop-executions (Loop-instr-list loop))])
       ;; (display possible-executions) (display "\n")

       (map (append-item (Assume-loop (Loop-condition loop) loop-id)) (filter not-empty (all-combinations possible-executions (unroll-loop loop (- depth 1) loop-id)))))]))
     
(define (range n)
  (if (= n 0)
      (list 0)
      (append (list n) (range (- n 1)))))


(define (possible-single-loop-executions l)
  ;; (display "possible loop executions\n")
  (cond
    [(empty? l) (list (list))]
    [(Single-branch? (first l))
     (append
      (list (list (Assume-simulation (Single-branch-condition (first l)))))
      (map (append-item (Assume-simulation (lambda (e) (not ((Single-branch-condition (first l)) e))))) (possible-single-loop-executions (rest l))))]
    [else
     (map (append-item (first l)) (possible-single-loop-executions (rest l)))]))
   


(define (all-unrolls loop depth loop-id)

  (define (give-ids-to-instr-list loop loop-id)
    ;; (display "assigning ids\n")
    ;; (display loop) (display "\n")
    
    (cond
      [(empty? loop) `()]
      [(Repeat-meta? (first loop))
       ;; (display "going into repeat meta\n")
       (append
        (list (Repeat-meta (give-ids-to-instr-list (Repeat-meta-instr-list (first loop)) loop-id) (Repeat-meta-which-var (first loop))))
        (give-ids-to-instr-list (rest loop) loop-id))]

      [(Single-branch? (first loop))
       ;; (display "going into single branch\n")
       (append
        (list (Single-branch (Single-branch-condition (first loop)) (give-ids-to-instr-list (Single-branch-branch (first loop)) loop-id)))
        (give-ids-to-instr-list (rest loop) loop-id))]

      [(Meta-addition? (first loop))
       (append
        (list (Meta-addition (give-ids-to-instr-list (Meta-addition-instr-list (first loop)) loop-id) (Meta-addition-which-var (first loop))))
        (give-ids-to-instr-list (rest loop) loop-id))]
                     
      [else
       (append
        (list (add-new-line-id (first loop) loop-id))
        (give-ids-to-instr-list (rest loop) loop-id))]))

                         

  (define (all-unrolls-helper loop depth)
    ;; (display "all unrolls\n")

    (cond
      [(> 0 depth) (list)]
      [else
       (append
        (unroll-loop loop depth loop-id)
        (all-unrolls-helper loop (- depth 1)))]))
  
  (let ([loop-with-ids (Loop (Loop-condition loop) (give-ids-to-instr-list (Loop-instr-list loop) loop-id))])
    (all-unrolls-helper loop-with-ids depth)))



(define thread1
  (Thread-list
   (list
    (Run-method "remove" (list 1 "test") "ret" ))))


(define thread2
  (Thread-list
   (list
    (Run-method "extension" (list 1 "test") "ret2" ))))


       
  
(define line-ids (void))
(define (add-new-line-id line loop-id)
  ;; (display "try adding line id to: ")(display loop-id) (display "\n")

  (cond
    [(Set-pointer? line)
     (if (None? (Set-pointer-instr-id line))
         (Set-pointer (Set-pointer-id line) (Set-pointer-type line) (Set-pointer-offset line) (Set-pointer-val line) (new-line-id))
         line)]
     
    [(Lock? line)
     (if (None? (Lock-instr-id line))
         (Lock (Lock-id line) (new-line-id))
         line)]
    [(Create-var? line)
     (if (None? (Create-var-instr-id line))
         (Create-var (Create-var-id line) (Create-var-type line) (new-line-id))
         line)]

    
    [(Set-var? line)
     (if (None? (Set-var-instr-id line))
         (Set-var (Set-var-id line) (Set-var-assignment line) (new-line-id))
         line)]

    [(Unlock? line)
     (if  (None? (Unlock-instr-id line))
         (Unlock (Unlock-id line) (new-line-id))
         line)]

    [(Continue? line)
     ;; (display "found a continue...\n")
     ;; (display "adding id ") (display loop-id) (display "\n")
     (if (and (not (None? loop-id)) (None? (Continue-to-where line)))
         (Continue loop-id)
         line)]
    [else
     line]))
(define (new-line-id)
  (set! line-ids (+ line-ids 1))
  line-ids)
      



(define meta-var-count (void))
(set! meta-var-count 0)

(define (new-meta-var)
  (set! meta-var-count (+ meta-var-count 1))
  meta-var-count)
  


(define loop-id-count (void))
(set! loop-id-count 0)

(define (new-loop-id)
  (set! loop-id-count (+ loop-id-count 1))
  loop-id-count)


(define (thread-runs t library t-id)

  (set! line-ids 0)


    

   

  (define (unroll-thread-runs instr-list to-return)
    ;; (display "thread-runs list: ")(display instr-list) (display "\n")
    (cond
      [(empty? instr-list) ;; (display "empty\n")
       (list (list))]
      [(Return? (first instr-list)) ;; (display "return\n")
       (list (list (Set-var to-return (Return-val (first instr-list)) (new-line-id) )))]
      [(Single-branch? (first instr-list)) ;; (display "single branch\n")
       (append
        (map (append-item (Assume-simulation (Single-branch-condition (first instr-list)))) (unroll-thread-runs (append (Single-branch-branch (first instr-list)) (rest instr-list)) to-return))
        (map (append-item (Assume-simulation (Not (Single-branch-condition (first instr-list))))) (unroll-thread-runs (rest instr-list) to-return)))]

      ;; [(Run-method? (first instr-list)) ;; (display "run method\n")
      ;;  (define arg-var-name (string-append "new-var" (~v (freshvar))))
      ;;  (define new-ctx-switch (Context-switch))
      ;;  (set-C-Instruction-thread-id! new-ctx-switch (C-Instruction-thread-id (first instr-list))) 
      ;;  (map (append-list
      ;;        (list
      ;;         new-ctx-switch
      ;;         (add-new-line-id (Create-var arg-var-name "int" (None) ) (None))
      ;;         (add-new-line-id (Set-var arg-var-name (Run-method-args (first instr-list)) (None)) (None))))


      ;;       (all-combinations (unroll-thread-runs (retrieve-code library (Run-method-method (first instr-list))) (Run-method-ret (first instr-list)))
      ;;                         (unroll-thread-runs (rest instr-list) to-return)))]




      [(Meta-addition? (first instr-list))
       (let ([new-meta (new-meta-var)])
         (append
          (map (append-item (Assume-meta new-meta)) (unroll-thread-runs (append (Meta-addition-instr-list (first instr-list))
                                                                                    (rest instr-list)) to-return))
          (map (append-item (Assume-not-meta new-meta)) (unroll-thread-runs (rest instr-list) to-return))))]
       

      [(Loop? (first instr-list)) ;; (display "looping\n")
       ;; (display "loop id: ") (display (new-loop-id)) (display "\n")
       ;; (display "looping at ") (display instr-list) (display "\n")
       ;; (display "inside: ") (display (Loop-instr-list (first instr-list))) (display "\n")
       ;; (display "unrolls: ") (display (all-unrolls (first instr-list) 2 (new-loop-id))) (display "\n")
       ;; (exit)
       
       (reduce append (map (lambda (l)  (unroll-thread-runs (append l (rest instr-list)) to-return)) (all-unrolls (first instr-list) 1 (new-loop-id))))]

      [(Repeat-meta? (first instr-list))
       (reduce append (map (lambda (l) (map (append-item (Repeat-meta l (Repeat-meta-which-var (first instr-list)))) (unroll-thread-runs (rest instr-list) to-return)))


                                (unroll-thread-runs (Repeat-meta-instr-list (first instr-list)) to-return)))]
       
      
      [else ;; (display "else\n")
            ;; (display (unroll-thread-runs (rest instr-list))) (display "\n")
       ;; (display (Get-instr-id (add-new-line-id (first instr-list)))) (display "\n")
       (map (append-item (add-new-line-id (first instr-list) (None))) (unroll-thread-runs (rest instr-list) to-return))]))
  
  
  
  




  (let ([instr-list (Thread-list-instr-list t)])
    (unroll-thread-runs instr-list "")))





(define (create-announcement-version instr-list)

  ;; (define (get-expression-derefs expr)
  ;;   (match expr
  ;;     [(Dereference id type offset)
  ;;      #t]
  ;;     [(Equal expr1 expr2)
  ;;      (or (get-expression-derefs expr1) (get-expression-derefs expr2))]
  ;;     [(Not expr) (get-expression-derefs expr)]
  ;;     [(Or expr1 expr2) (or (get-expression-derefs expr1) (get-expression-derefs expr2))]
  ;;     [(And expr1 expr2) (or (get-expression-derefs expr1) (get-expression-derefs expr2))]
  ;;     [else
  ;;      `#f]))

  ;; (define (collect-dereferences instr-list)
  ;;   (cond
  ;;     [(empty? instr-list) `()]
  ;;     [else
  ;;      (match (first instr-list)
  ;;        [(Set-var id assignment instr-id)
  ;;         (append (get-expression-derefs assignment)
  ;;                 (create-announcement-version (rest instr-list)))]
  ;;        [else
  ;;         (create-announcement-version (rest instr-list))])]))
       

  (define (get-type expr)
    (cond 
      [(Or? expr) "int"]
      [(And? expr) "int"]
      [(Equal? expr) "int"]
      [(Dereference? expr) (Dereference-type expr)]
      [else (display "weird type issue\n")
       "int"]))

  (define (collect-all-assignments instr-list)
    (cond
      [(empty? instr-list) `()]
      [else
       (match (first instr-list)
         [(Set-var thread-id id assignment instr-id)
          (append (list (Tuple id (get-type assignment)))
                  (collect-all-assignments (rest instr-list)))]
         [(Loop thread-id condition loop-instr-list)
          (append (collect-all-assignments loop-instr-list) (collect-all-assignments (rest instr-list)))]
         [_
          `(collect-all-assignments (rest instr-list))])]))

  
  (define (announcement-sketch instr-list)
    (cond
      [(empty? instr-list) `()]
      [(match (first instr-list)
         [(Set-var thread-id id assignment instr-id)
          (if (Dereference? assignment)
              (append
               (list (Meta-addition (respond-to-announcements (Dereference-id assignment) (Dereference-type assignment)) (new-meta-var)))
               (announcement-sketch (rest instr-list)))
              (append (list (first instr-list)) (announcement-sketch (rest instr-list))))]


         [(Loop thread-id condition loop-instr-list)
          (let ([in-loop-assignments (collect-all-assignments loop-instr-list)])
            ;; (display in-loop-assignments) (display "\n")
            (append
             (list (first instr-list))
             (map (lambda (t)  (Meta-addition (respond-to-announcements (Tuple-a t) (Tuple-b t)) (new-meta-var)))
                  in-loop-assignments)
             (announcement-sketch (rest instr-list))))]
          
          
         [_
          (append (list (first instr-list)) (announcement-sketch (rest instr-list)))])]))
     


  

   ;; (Create-var "loop-break" "int" (None))
   ;; (Set-var "loop-break" #f (None))
   ;; (Loop (Not (Get-var "loop-break"))
   (announcement-sketch instr-list))



(define (respond-to-announcements node-of-interest type)
  (list

   (Single-branch (Not (Is-none? (Get-var node-of-interest)))
                  (list
                   (Create-var "bits" "int" (None))


                   (Set-var "bits" (Dereference node-of-interest type "bits") (None))
                   
                   
                   (Repeat-meta
                    (list
                     (Meta-addition
                      
                      (list
                       ;; ;; (Single-branch (Equal (Get-var "bits") (Mystery-const))
                       ;; ;;                (list
                       ;; ;;                 (Continue (None))))
                       
                       (Create-var "new-bits" "int" (None))
                       (Create-var "checkCAS" "int" (None))
                       (Set-var "new-bits" (new-meta-var) (None))
                       
                       (CAS (Dereference node-of-interest type "bits") (Get-var "bits") (Get-var "new-bits") "checkCAS")
                       
                       (Single-branch (Not (Get-var "checkCAS"))
                                      (list
                                       (Continue (None)))))
                      
                      ;; (Set-var "loop-break" #t (None)))
                      
                     
                      (new-meta-var))
                                    
             
                     ) (new-meta-var))))))


(define library
  (list
   
   (Method
    "get"
    ;; (add-ids add-method-id
    (list "Node" "int")
    "int"
    (create-announcement-version 
     (list
      (Lock 1(None))
      (Create-var "cur" "Node" (None))
      (Set-var "cur" (Get-argument 0)(None))
      (Loop  (And (Not (Is-none? (Get-var "cur"))) (Not (Equal (Dereference "cur" "Node" "key") (Get-argument 1))) )
            (list
             (Set-var "cur" (Dereference "cur" "Node" "next") (None))))
      (Single-branch
        (Is-none? (Get-var "cur"))
       (list
        (Unlock 1(None))
        (Return 0(None))))
      (Unlock 1(None))
      (Return (Dereference "cur" "Node" "val")(None)))
     ;; 0))
    ))
    
    
    (Method
     "contains"

     (list "Node" "int")
     "int"
     ;; (add-ids add-method-id
      (list
       (Create-var "val" "int"(None))
       (Run-method "get" (list (Get-argument 0) (Get-argument 1)) "val") ;; Run method "get" with arguments "key"
       (Return (Equal (Get-var "val") 0)(None)))
      ;; 1))
     )
    
   (Method
    "remove"
    (list "Node" "int")
    "int"
    ;; (add-ids add-method-id
     (list
      (Lock 1(None))
      (Create-var "cur" "Node"(None))
      (Create-var "prevNode" "Node"(None))
      (Create-var "oldVal" "Node"(None))
      
      (Set-var "cur" (Get-argument 0)(None))
      (Single-branch 
                     (Equal (Get-var "cur") 0)
                     (list
                      (Unlock 1(None))
                      (Return 0(None))))
      
      
      (Set-var "oldVal" (Dereference "cur" "Node" "val")(None))
      (Set-var "prevNode" (Get-argument 0)(None))
     
      (Loop  (And (Not (Equal (Dereference (Dereference "cur" "Node" "next") "Node" "key") (Get-argument 1))) (Not (Equal (Get-var "cur") 0)))
            (list
             (Set-var "oldVal" (Dereference "cur" "Node" "val")(None))
             (Set-var "prevNode" (Get-var "cur")(None))           
             (Set-var "cur" (Dereference "cur" "Node" "next")(None))))
      (Single-branch 
                     (Equal (Get-var "cur") 0)
                     (list
                      (Unlock 1(None))
                      (Return 0(None))))
      
      (Set-var "oldVal" (Dereference "cur" "Node" "val")(None))
      (Set-pointer "prevNode" "Node" "next" (Dereference "cur" "Node" "next")(None))
      (Unlock 1(None))
      (Return (Get-var "oldVal")(None)))
     ;; 2))
    )


   (Method
    "extension"
    ;; (add-ids add-method-id
    (list "Node" "int")
    "int"
     (list
      (Create-var "val" "int"(None))
      (Create-var "found" "int"(None))
      (Set-var "val" 0(None))
      (Run-method "contains" (list (Get-argument 0) (Get-argument 1)) "found")
      (Single-branch 
                     (Equal (Get-var "None") 0)
                     (list
                      (Run-method "get" (list (Get-argument 0) (Get-argument 1)) "val")
                      (Run-method "remove" (list (Get-argument 0) (Get-argument 1)) "")))
      (Return (Get-var "val")(None)))
    )))
     
     ;; 3))))

    




 ;; (interleavings thread1 thread2 library (list (list 0 1  2 3 4 5) ))

(define (Get-thread-id x)
  (match x
    [(Info t-id instr-id) t-id]
    [_ (None)]))


(define (Get-instr-id x)
  (match x
    [(Lock thread-id id instr-id) instr-id]
    [(Create-var thread-id id type instr-id) instr-id]
    [(Unlock thread-id id instr-id) instr-id]
    [(Return thread-id val instr-id) instr-id]
    [(Set-pointer id type offset val instr-id) instr-id]
    [(Set-var thread-id id assignment instr-id) instr-id]
    [(Info  t-id instr-id) instr-id]
    [_
     (None)]))


;; (define (Get-info x)
;;   0)



(define (find-appropriate-subseq interleaving instr)

  (define (helper interleaving instr original)
    ;; (display "finding subinterleaving for: ")(display (Get-instr-id instr)) (display "\n")
    (cond
      [(None? (Get-instr-id instr))
       original]
      [(empty? interleaving)
       original]
      [else
       (if (equal? (Get-instr-id instr) (first interleaving))
           interleaving
           (helper (rest interleaving) instr original))]))
  (helper interleaving instr interleaving))
    
  

(define (all-equivalent-interleavings interleaving-full thread1 thread2 library)

  (define (all-equivalent-interleavings-helper interleavings thread1-runs thread2-runs)
    (display "all-equivalent-interleavings-helper\n")
    (cond
      [(empty? interleavings)
       (list (list))]
      [else
       (let ([t-id (Get-thread-id (first interleavings))]
             [same-id-thread1 (filter (lambda (l) (and (not (empty? l)) (equal? (Get-instr-id (first l))
                                                                                (Get-instr-id (first interleavings)))))
                                      thread1-runs)]
             [same-id-thread2 (filter (lambda (l) (and (not (empty? l)) (equal? (Get-instr-id (first l))
                                                                                (Get-instr-id (first interleavings)))))
                                      thread2-runs)]

             [maybe-loop-thread1 (filter (lambda (l) (and (not (empty? l)) (None? (Get-instr-id (first l)))))
                                         thread1-runs)]
             [maybe-loop-thread2 (filter (lambda (l) (and (not (empty? l)) (None? (Get-instr-id (first l)))))
                                         thread2-runs)])
         


         ;; (display "same-id-thread1: ") (display  (map (lambda (l) (rest l)) same-id-thread1)) (display "\n")
         ;; (display "maybe-loop-thread1: ") (display maybe-loop-thread1) (display "\n")




         (cond
           [ (equal? t-id 0) 
            (append
             (map (lambda (l) (if (empty? l) `()
                                  (append (list (first l))

                                          (first (all-equivalent-interleavings-helper
                                                            (rest interleavings)
                                                            (list (rest l))
                                                            thread2-runs)))))
                  same-id-thread1)




             (map (lambda (l)
                    (display l) (display "\n")
                    (if (> 2 (length l)) `()
                        (all-equivalent-interleavings-helper (find-appropriate-subseq interleaving-full (first (rest l)))
                                                  (rest (list l))
                                                  thread2-runs)))
                  maybe-loop-thread1))]
           
           [else
            (list (list))]))]))

  
            



  
  (let ([thread1-runs (thread-runs thread1 library 0)]
        [thread2-runs (thread-runs thread2 library 1)])

    (all-equivalent-interleavings-helper interleaving-full thread1-runs thread2-runs)))






   




(define thread3
  (Thread-list
   (list
    (Run-method "get" (list 1 "test") "ret" ))))



(define test
  (Thread-list
   (list
    ;; (Create-var "ret1" "int" (None))
    (Create-var "ret2" "int" (None))
    (Create-var "ret3" "int" (None))    
    (Run-Method "get" (list (Get-var "shared") 1 ) "ret3" 0)
    (Run-Method "contains" (list (Get-var "shared") 1) "ret2" 1)
    (Run-Method "remove" (list (Get-var "shared") 1) "ret1" 0))))
  







(define (collect-meta-vars interleaving library)
  (list (Tuple "meta-var3" "integer?") (Tuple "meta-var4" "integer?") (Tuple "meta-var1" "integer?") (Tuple "meta-var2" "boolean?")))

(define (interleaving-to-sketch interleaving variables-of-interest library)
  (define count-branches (void))
  (set! count-branches 0)
  


  (define (remove-global-declarations str l)
    (cond
      [(empty? l) str]
      [else
       (remove-global-declarations (string-replace str (first l) "\n") (rest l))]))


  (define prelude
"
#lang rosette
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
(set! shared (Node  (None)(None) (None) (None)))



(define pick-trace 1)
")

  
  (let ([all-runs (thread-runs interleaving library 0)]
        [meta-vars (collect-meta-vars interleaving library)])



    ;; (display (length all-runs)) (display "\n")


(string-append
   prelude
  (reduce string-append (map (lambda (v) (string-append "(define " v " (void))\n")) variables-of-interest))

  (remove-global-declarations


 
   (string-append

    ;; All symbolic variables which will be referenced in the possible traces 
    (reduce string-append
            (map (lambda (v) (string-append "(define-symbolic " (Tuple-a v) " " (Tuple-b v) ")\n"))
                 meta-vars))
    

    "(cond\n"
    
    (reduce string-append
            (map (lambda (l) (set! count-branches (+ count-branches 1)) (string-append "[(equal? pick-trace " (~v count-branches) ") (begin\n" (instr-list-to-sketch l library `()) ")]\n"))
                 all-runs))
    
    ")\n")
   
   (map (lambda (v) (string-append "(define " v " (void))")) variables-of-interest))


  "(list " (reduce string-append (map (lambda (v) (string-append "(cons \"" v "\" " v ") ")) variables-of-interest)) ")")))

    

    






;; (first (thread-runs thread3 library 0))
;; (to-string-instr (Set-var-assignment (second (first (thread-runs thread3 library 0)))))


;; (first (thread-runs thread3 library 0))

;; (display (print-non-sketch-simulation (list (Run-method "get" (list 1 "test" ) "aret")) library (list 1 2 3)  "default"))

;; (display (instr-list-to-sketch (first (thread-runs test library 0)) library "args"))

(display (interleaving-to-sketch test (list "ret1" "ret2" "ret3") library))
;; (length (thread-runs test library 0)
;; (first (thread-runs test library 0))

;; (second (thread-runs thread1 library 0))
;; (map Get-instr-id (second (thread-runs thread1 library 0)))


;; (first (all-equivalent-interleavings (list (Info 0 1) (Info 0 2) (Info 0 3) (Info 0 4) (Info 0 5) (Info 0 6) (Info 0 7) (Info 0 15) (Info 0 16) ) thread1 thread2 library))





