#lang racket

(require "../program_representation/simulator-structures.rkt")
(require racket/string)

(require "to-sketch.rkt")
(require "metasketcher.rkt")

(provide interleaving-to-sketch
         create-announcement-version
         )
 
(define counter (void))
(set! counter 0)
(define (freshvar)
  (set! counter (+ counter 1))
  counter)


(define (unique comparison l)
  (cond
    [(empty? l) `()]
    [else
     (append (list (first l))
             (filter (lambda (i) (not (comparison (first l) i))) (unique comparison (rest l))))]))

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
    [(equal? depth 0)  (list (list (Assume-loop 
                                                  (not (Loop-condition loop)) loop-id)))]
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
    loop)
    ;; (cond
    ;;   [(empty? loop) `()]
    ;;   [(Repeat-meta? (first loop))
    ;;    ;; (display "going into repeat meta\n")
    ;;    (append
    ;;     (list (Repeat-meta (give-ids-to-instr-list (Repeat-meta-instr-list (first loop)) loop-id) (Repeat-meta-which-var (first loop))))
    ;;     (give-ids-to-instr-list (rest loop) loop-id))]

    ;;   [(Single-branch? (first loop))
    ;;    ;; (display "going into single branch\n")
    ;;    (append
    ;;     (list (Single-branch (Single-branch-condition (first loop)) (give-ids-to-instr-list (Single-branch-branch (first loop)) loop-id)))
    ;;     (give-ids-to-instr-list (rest loop) loop-id))]

    ;;   [(Meta-addition? (first loop))
    ;;    (append
    ;;     (list (Meta-addition (give-ids-to-instr-list (Meta-addition-instr-list (first loop)) loop-id) (Meta-addition-which-var (first loop))))
    ;;     (give-ids-to-instr-list (rest loop) loop-id))]
                     
    ;;   [else
    ;;    (append
    ;;     (list (add-new-line-id (first loop) loop-id))
    ;;     (give-ids-to-instr-list (rest loop) loop-id))]))

                         

  (define (all-unrolls-helper loop depth)
    ;; (display "all unrolls\n")

    (cond
      [(> 0 depth) (list)]
      [else
       (append
        (unroll-loop loop depth loop-id)
        (all-unrolls-helper loop (- depth 1)))]))

  (cond
    [(Loop? loop)
     (let ([loop-with-ids (Loop (Loop-condition loop) (give-ids-to-instr-list (Loop-instr-list loop) loop-id))])
       (all-unrolls-helper loop-with-ids depth))]
    [(Maybe-loop? loop)
     (let ([loop-with-ids (Loop (Maybe-loop-condition loop) (give-ids-to-instr-list (Maybe-loop-instr-list1 loop) loop-id))])
       (all-unrolls-helper loop-with-ids depth))]

    [else
     (displayln loop)
     (displayln "Need loop or maybe loop. Something is wrong")]))



(define thread1
  (Thread-list
   (list
    (Run-method "remove" (list 1 "test") "ret" ))))


(define thread2
  (Thread-list
   (list
    (Run-method "extension" (list 1 "test") "ret2" ))))


       
  
(define line-ids (void))
;; (define (add-new-line-id line loop-id)
;;   ;; (display "try adding line id to: ")(display loop-id) (display "\n")
;;   (
  ;; (cond
  ;;   [(Set-pointer? line)
  ;;    (if (None? (Set-pointer-instr-id line))
  ;;        (Set-pointer (Set-pointer-id line) (Set-pointer-type line) (Set-pointer-offset line) (Set-pointer-val line) (new-line-id))
  ;;        line)]
     
  ;;   [(Lock? line)
  ;;    (if (None? (Lock-instr-id line))
  ;;        (Lock (Lock-id line) (new-line-id))
  ;;        line)]
  ;;   [(Create-var? line)
  ;;    (if (None? (Create-var-instr-id line))
  ;;        (Create-var (Create-var-id line) (Create-var-type line) (new-line-id))
  ;;        line)]

    
  ;;   [(Set-var? line)
  ;;    (if (None? (Set-var-instr-id line))
  ;;        (Set-var (Set-var-id line) (Set-var-assignment line) (new-line-id))
  ;;        line)]

  ;;   [(Unlock? line)
  ;;    (if  (None? (Unlock-instr-id line))
  ;;        (Unlock (Unlock-id line) (new-line-id))
  ;;        line)]

  ;;   [(Continue? line)
  ;;    ;; (display "found a continue...\n")
  ;;    ;; (display "adding id ") (display loop-id) (display "\n")
  ;;    (if (and (not (None? loop-id)) (None? (Continue-to-where line)))
  ;;        (Continue loop-id)
  ;;        line)]
  ;;   [else
  ;;    line]))
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

(define (interleave-lists instr-list1 instr-list2)
  (cond
    [(empty? instr-list1) (list instr-list2)]
    [(empty? instr-list2) (list instr-list1)]
    [else
     (append
      (map (append-item (first instr-list1)) (interleave-lists (rest instr-list1) instr-list2))
      (map (append-item (first instr-list2)) (interleave-lists instr-list1 (rest instr-list2))))]))




(define (interleave-lists-marker instr-list1 instr-list2)
  (define (helper instr-list1 instr-list2 ctx-switch-enabled found-marker)
    (cond
      [(empty? instr-list1) (list instr-list2)]
      [(empty? instr-list2) (list instr-list1)]
      [(Added-CAS-Marker? (first instr-list2))
       (helper instr-list1 (rest instr-list2) #t #t)]
      [(and found-marker (RW-operation (first instr-list2)))
       (append
        (map (append-item (first instr-list1)) (helper (rest instr-list1) instr-list2 #t #t))
        (map (append-item (first instr-list2)) (helper instr-list1 (rest instr-list2) #t #t)))]
      [(and found-marker (not (RW-operation (first instr-list2))))
        (map (append-item (first instr-list2)) (helper instr-list1 (rest instr-list2) #t #t))]
      [ctx-switch-enabled
       (append
        (map (append-item (first instr-list1))
             (helper (rest instr-list1) instr-list2 #f #f))
        (map (append-item (first instr-list2))
             (helper instr-list1 (rest instr-list2) #f #f)))]
      [else
        (map (append-item (first instr-list2)) (helper instr-list1 (rest instr-list2) ctx-switch-enabled found-marker))]))
  (helper instr-list1 instr-list2 #t #f))
        
        


(define (interleave-lists-at-holes instr-list1 instr-list2 hole)
  ;; (display "INTERLEAVING LIST1: ") (displayln instr-list1)
  ;; (display "INTERLEAVING LIST2: ") (displayln instr-list2)
  
  (cond
    [(empty? instr-list1) (list instr-list2)]
    [(empty? instr-list2) (list instr-list1)]
    [(equal? (C-Instruction-instr-id (first instr-list1)) (Hole-method1 hole))
     (append
      (map (append-list (list (first instr-list1) (first instr-list2))) (interleave-lists-at-holes (rest instr-list1) (rest instr-list2) hole))

      (map (append-item (first instr-list1)) (interleave-lists-at-holes (rest instr-list1) instr-list2 hole)))]
    [else
     (map (append-item (first instr-list1)) (interleave-lists-at-holes (rest instr-list1) instr-list2 hole))]))

(define (thread-runs t library t-id to-return)

  (set! line-ids 0)


    
  (define (handle-maybe-loop instr-list to-return)

    (let
        ([maybe-loop (first instr-list)]
         [possible-loops (reduce append (map (lambda (l)  (unroll-thread-runs (append l (rest instr-list)) to-return)) (all-unrolls (first instr-list) 1 (new-loop-id))))])

      (let
          ([interleavings (reduce append
                                  (map
                                   (lambda (l)
                                     ;; (displayln "one loop possibility\n")
                                     ;; (display "answer: ") (displayln                                     (length (interleave-lists-at-holes l (Maybe-loop-instr-list2 maybe-loop) (Maybe-loop-hole maybe-loop))))

                                     (interleave-lists-at-holes l (Maybe-loop-instr-list2 maybe-loop) (Maybe-loop-hole maybe-loop)))
                                   possible-loops))])

        ;; (display "INTERLEAVINGS: ") (displayln  (first interleavings))
        
        

        

      ;; (display "POSSIBLE-LOOPS: ")
      ;; (displayln (second (interleave-lists-at-holes (first possible-loops) (Maybe-loop-instr-list2 maybe-loop) (Maybe-loop-hole maybe-loop))))

      
      interleavings)))

   

  (define (unroll-thread-runs instr-list to-return)
    ;; (display "thread-runs list: ")(display instr-list) (display "\n")
    (cond
      [(empty? instr-list) ;; (display "empty\n")
       (list (list))]
      [(Return? (first instr-list)) ;; (display "return ") (displayln (~v to-return))
       (list (list (Set-var to-return (Return-val (first instr-list)) )))]
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

      [(Meta-branch? (first instr-list))
       ;; (display "meta-branch\n")
       ;; (displayln           (length (map (append-item (Assume-meta 0)) (unroll-thread-runs (append
       ;;                                                                   (Meta-branch-branch1 (first instr-list))
       ;;                                                                   (rest instr-list))
       ;;                                                                  to-return))))
       ;; (displayln           (length (map (append-item (Assume-not-meta 0)) (unroll-thread-runs (append
       ;;                                                                       (Meta-branch-branch2 (first instr-list))
       ;;                                                                       (rest instr-list))
       ;;                                                                      to-return))))

 
       (let ([new-meta (new-meta-var)])
         (append
          (map (append-item (Assume-meta new-meta)) (unroll-thread-runs (append
                                                                         (Meta-branch-branch1 (first instr-list))
                                                                         (rest instr-list))
                                                                        to-return))
          (map (append-item (Assume-not-meta new-meta)) (unroll-thread-runs (append
                                                                             (Meta-branch-branch2 (first instr-list))
                                                                             (rest instr-list))
                                                                            to-return))))]


          
       

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
       




      [(Maybe-loop? (first instr-list))
       (let
           ([new-meta (new-meta-var)])
         (append
          (map (append-item (Assume-meta new-meta))
                (handle-maybe-loop instr-list to-return))


          (map (append-item (Assume-not-meta new-meta))
               (unroll-thread-runs
                (append (Maybe-loop-original-instr-list (first instr-list))
                        (rest instr-list))
                to-return))))]

               
      
      [else ;; (display "else\n")
            ;; (display (unroll-thread-runs (rest instr-list))) (display "\n")
       ;; (display (Get-instr-id (add-new-line-id (first instr-list)))) (display "\n")
       (map (append-item  (first instr-list)) (unroll-thread-runs (rest instr-list) to-return))]))
  
  
  
  




  (let ([instr-list (Thread-list-instr-list t)])
    (unroll-thread-runs instr-list to-return)))










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
         [(Set-var thread-id instr-id id assignment)
          (append (list (Tuple id (get-type assignment)))
                  (collect-all-assignments (rest instr-list)))]
         [(Loop thread-id instr-id condition loop-instr-list)
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


         [(Loop thread-id instr-id condition loop-instr-list)
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


                   (Set-var "bits" (Dereference node-of-interest type "bits"))
                   
                   
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
    "push"
    (list "Node" "int" "int")
    "int"
    (list
     (Lock 1 )
     (Create-var "cur" "Node" )
     (Create-var "prev" "Node" )
     (Set-var "cur" (Get-argument 0) )
     (Set-var "prev" (Get-argument 0) )
     (Loop (And (Not (Is-none? (Get-var "cur"))) (Not (Equal (Dereference "cur" "Node" "key") (Get-argument 1))))
           (list
            (Set-var "prev" (Get-var "cur") )
            (Set-var "cur" (Dereference "cur" "Node" "next") )))
     (Single-branch
      (Is-none? (Get-var "cur"))
      (list
       (Set-pointer "prev" "Node" "next" (New-struct "Node" (list (None) (Get-argument 1) (Get-argument 2) (None) )) )
       (Unlock 1 )
       (Return (Get-argument 2) )))

     (Set-pointer "cur" "Node" "val" (Get-argument 2) )
     (Unlock 1 )
     (Return (Get-argument 2) )

     ))
     


   (Method
    "get"
    ;; (add-ids add-method-id
    (list "Node" "int")
    "int"
    ;; (create-announcement-version 
     (list
      (Lock 1)
      (Create-var "cur" "Node" )
      (Set-var "cur" (Get-argument 0))
      (Loop  (And (Not (Is-none? (Get-var "cur"))) (Not (Equal (Dereference "cur" "Node" "key") (Get-argument 1))) )
            (list
             (Set-var "cur" (Dereference "cur" "Node" "next") )))
      (Single-branch
        (Is-none? (Get-var "cur"))
       (list
        (Unlock 1)
        (Return (None))))
      (Unlock 1)
      (Return (Dereference "cur" "Node" "val")))
     ;; 0))
    ;; ))
     )
    
    
    (Method
     "contains"

     (list "Node" "int")
     "int"
     ;; (add-ids add-method-id
      (list
       (Create-var "val" "int")
       (Run-method "get" (list (Get-argument 0) (Get-argument 1)) "val") ;; Run method "get" with arguments "key"
       (Return (Not (Is-none? (Get-var "val"))) ))
      ;; 1))
     )
    
   (Method
    "remove"
    (list "Node" "int")
    "int"
    ;; (add-ids add-method-id
     (list
      (Lock 1)
      (Create-var "cur" "Node")
      (Create-var "prevNode" "Node")
      (Create-var "oldVal" "Node")
      
      (Set-var "cur" (Get-argument 0))
      (Single-branch 
                     (Is-none? (Get-var "cur"))
                     (list
                      (Unlock 1)
                      (Return  (None))))
      
      
      (Set-var "oldVal" (Dereference "cur" "Node" "val"))
      (Set-var "prevNode" (Get-argument 0))
     
      (Loop  (And (Not (Is-none? (Get-var "cur"))) (Not (Equal (Dereference "cur" "Node" "key") (Get-argument 1))))

             ;; (And (Not (Equal (Dereference (Dereference "cur" "Node" "next") "Node" "key") (Get-argument 1))) (Not (Equal (Get-var "cur") 0)))
            (list
             (Set-var "oldVal" (Dereference "cur" "Node" "val"))
             (Set-var "prevNode" (Get-var "cur"))           
             (Set-var "cur" (Dereference "cur" "Node" "next"))))

      (Added-CAS-Marker)
      (Single-branch 
       (Is-none? (Get-var "cur"))
       (list
        (Unlock 1)
        (Return  (None))))
      

      (Set-var "oldVal" (Dereference "cur" "Node" "val"))
      (Set-pointer "prevNode" "Node" "next" (Dereference "cur" "Node" "next"))
      (Unlock 1)
      (Return (Get-var "oldVal")))
     ;; 2))
    )


   (Method
    "extension"
    ;; (add-ids add-method-id
    (list "Node" "int")
    "int"
     (list
      (Create-var "val" "int" )
      (Create-var "found" "int" )
      (Set-var "val" (None) )
      (Run-method "contains" (list (Get-argument 0) (Get-argument 1)) "found")
      (Single-branch 
       (Get-var "found") 
       (list
        (Run-method "get" (list (Get-argument 0) (Get-argument 1)) "val")
        (Run-method "remove" (list (Get-argument 0) (Get-argument 1)) "throwaway")))
      (Return (Get-var "val")))
    )))
     
     ;; 3))))

    




 ;; (interleavings thread1 thread2 library (list (list 0 1  2 3 4 5) ))

(define (Get-thread-id x)
  (match x
    [(Info t-id instr-id) t-id]
    [_ (None)]))




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

  
            



  
  (let ([thread1-runs (thread-runs thread1 library 0 "")]
        [thread2-runs (thread-runs thread2 library 1 "")])

    (all-equivalent-interleavings-helper interleaving-full thread1-runs thread2-runs)))






   
(define (transform-to-traces l)
  (define (helper l n)
    (cond
      [(empty? l)
       `()]
      [else
       (append
        (list (Trace n (first l)))
        (helper (rest l) (+ n 1)))]))
      (helper l 0))
     



(define thread3
  (Thread-list
   (list
    (Run-method "get" (list 1 "test") "ret" ))))



(define test
  (Thread-list
   (list
    ;; (Create-var "ret1" "int" (None))
    (Create-var "ret2" "int" )
    (Create-var "ret3" "int" )
    (Run-Method "push" (list (Get-var "shared") 1 5) "ret1" 0 )
    (Run-Method "push" (list (Get-var "shared") 2 5) "ret1" 0 )
    (Run-Method "push" (list (Get-var "shared") 3 7) "ret1" 0 )
    (Run-Method "get" (list (Get-var "shared") 2 ) "ret3" 0)
    ;; (Run-Method "contains" (list (Get-var "shared") 1) "ret2" 1)
    (Run-Method "remove" (list (Get-var "shared") 1) "ret1" 0))))
  






;;; Mooly's error
(define mooly-test
  (Thread-list
   (list
    (Run-Method "push" (list (Get-argument 0) 1 5) "throwaway" 0)
    (Create-var "val" "int" )
    (Create-var "found" "int" )
    (Set-var "val" 0 )
    (Run-Method "contains" (list (Get-argument 0) (Get-argument 1)) "found" 0)
    (Assume-simulation (Not (Equal (Get-var "found") 0)))

    
    (Run-Method "get" (list (Get-argument 0) (Get-argument 1)) "val" 0)
    (Run-Method "remove" (list (Get-argument 0) (Get-argument 1)) "ret2" 1)
    (Run-Method "remove" (list (Get-argument 0) (Get-argument 1)) "throwaway" 0)
    (Set-var "ret1" (Get-var "val") ))))



(define mooly-sketch-test
  (Thread-list
   (list
    (Run-Method "push" (list (Get-argument 0) 1 5) "throwaway" 0)
    (Create-var "val" "int" )
    (Create-var "found" "int" )
    (Set-var "val" 0 )
    (Run-Method "contains" (list (Get-argument 0) (Get-argument 1)) "found" 0)
    (Assume-simulation (Not (Equal (Get-var "found") 0)))

    (Meta-branch 0
                 (list
                  (Run-Method "get" (list (Get-argument 0) (Get-argument 1)) "val" 0)
                  (Run-Method "remove" (list (Get-argument 0) (Get-argument 1)) "ret2" 1)
                  (Run-Method "remove" (list (Get-argument 0) (Get-argument 1)) "throwaway" 0))
                 (list
                  (Create-var "loop-break" "int" )
                  (Set-var "loop-break" #f )
                  (Maybe-loop 1 (Not (Get-var "loop-break"))
                              (list
                               (Run-Method-instr-id "get" (list (Get-argument 0) (Get-argument 1)) "val" 0 0)
                               (Run-Method-instr-id "remove" (list (Get-argument 0) (Get-argument 1)) "throwaway" 0 1))
                              (list
                               (Run-Method "remove" (list (Get-argument 0) (Get-argument 1)) "ret2" 1))

                              (list
                               (Run-Method "get" (list (Get-argument 0) (Get-argument 1)) "val" 0)
                               (Run-Method "remove" (list (Get-argument 0) (Get-argument 1)) "ret2" 1)
                               (Run-Method "remove" (list (Get-argument 0) (Get-argument 1)) "throwaway" 0))



                              (Hole 0 (list) 1))))

                              

                              
                        

                 
    (Set-var "ret1" (Get-var "val")))))











    
    ;; (#(struct:Create-var () val int #<None>)
    ;;  #(struct:Create-var () found int #<None>)
    ;;  #(struct:Set-var () val 0 #<None>)
    ;;  #(struct:Run-method () contains (#(struct:Get-argument () 0) #(struct:Get-argument () 1)) found)
    ;;  #(struct:Run-method #t get (#<Get-var> 15) g87121)
    ;;  #(struct:Run-method #t get (#<Get-var> 15) g87121)
    ;;  #(struct:Single-branch () #<Equal> (#(struct:Run-method () get (#(struct:Get-argument () 0) #(struct:Get-argument () 1)) val)
    ;;                                      #(struct:Run-method () get (#(struct:Get-argument () 0) #(struct:Get-argument () 1)) )))
    ;;  #(struct:Return () #<Get-var> #<None>))



(define (collect-meta-vars interleaving-set library)
  (define (interleaving-get-meta-vars interleaving)

    (cond
      [(empty? interleaving) `()]
      [(Assume-meta? (first interleaving)) (append (list

                                                    (Tuple
                                                     (string-append "meta-var" (~v (Assume-meta-condition (first interleaving))))
                                                     "boolean?"))
                                                   (interleaving-get-meta-vars (rest interleaving)))]
      [(Assume-not-meta? (first interleaving)) (append (list
                                                        (Tuple
                                                         (string-append "meta-var" (~v (Assume-not-meta-condition (first interleaving))))
                                                         "boolean?"))
                                                       (interleaving-get-meta-vars (rest interleaving)))]
      [else
       (interleaving-get-meta-vars (rest interleaving))]))

  
  ;; (list (Tuple "meta-var3" "integer?") (Tuple "meta-var4" "integer?") (Tuple "meta-var1" "integer?") (Tuple "meta-var2" "boolean?")))

  (reduce append
          (map (lambda (l) (interleaving-get-meta-vars l))
               interleaving-set)))

(define (interleaving-to-sketch interleaving variables-of-interest library hole)
  (define count-branches (void))
  (set! count-branches 0)
  


  (define (remove-global-declarations str l)
    (cond
      [(empty? l) str]
      [else
       (remove-global-declarations (string-replace str (first l) "\n") (rest l))]))


  (define prelude "
    
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
(set! shared (Node  (None) \"test\" \"testval\" (None)))


(define first-args (void))
(set! first-args (list shared 1))


(define pick-trace 1)")



  (let ([all-runs (reduce append (map (lambda (l) (convert-trace-to-interleavings l library (list "remove") hole)) (thread-runs interleaving library 0 "")))])
        (let ([meta-vars (unique (lambda (fst snd) ;; (displayln (equal? (Tuple-a fst) (Tuple-a snd)))

 (equal? (Tuple-a fst) (Tuple-a snd))) (collect-meta-vars all-runs library))]
[new-scope (new-scope-num)])


(string-append
 prelude
 ;; (generate-library-code library)
 
 (trace-list-to-sketch (transform-to-traces all-runs) library "first-args" new-scope 0)))))


;; (display "ALL-RUNS length: ") (displayln (length all-runs))
;; (display "META-VARS: ") (displayln meta-vars)
;; (displayln (first all-runs))





;; (display (length all-runs)) (display "\n")



;;; OLD VERSION BEGINS HERE
;; (add-binding-parent new-scope 0)
;; (string-append
;;    prelude
;;   (reduce string-append (map (lambda (v) (string-append "(define " v " (void))\n")) variables-of-interest))

;;   (remove-global-declarations


 
;;    (string-append

;;     ;; All symbolic variables which will be referenced in the possible traces 
;;     (reduce string-append
;;             (map (lambda (v) (string-append "(define-symbolic " (Tuple-a v) " " (Tuple-b v) ")\n"))
;;                  meta-vars))
    

;;     "(cond\n"
    
;;     (reduce string-append
;;             (map (lambda (l) (set! count-branches (+ count-branches 1)) (string-append "[(equal? pick-trace " (~v count-branches) ") (begin\n" (instr-list-to-sketch l library "first-args" new-scope 0) ")]\n"))
;;                  all-runs))
    
;;     ")\n")
   
;;    (map (lambda (v) (string-append "(define " v " (void))")) variables-of-interest))


;;   "(list " (reduce string-append (map (lambda (v) (string-append "(cons \"" v "\" " v ") ")) variables-of-interest)) ")"))))

    

(define (list-contains l i)
  (> (length (filter (lambda (e) (equal? e i)) l)) 0))



(define (exists-write-in-hole instr-list hole)
  (define (helper l looking)
    (cond
      [(empty? l) #f]
      [(and looking
            (or (Set-pointer? (first l))
                ;; (CAS? (first l))
                ;; (and (Set-var? (first l)) (Dereference? (Set-var-assignment (first l))))))
                ))
       #t]
      [(equal? (C-Instruction-instr-id (first l)) (Hole-method1 hole))
       (helper (rest l) #t)]
      [(equal? (C-Instruction-instr-id (first l)) (Hole-method2 hole))
       (helper (rest l) #f)]
      [else
       (helper (rest l) looking)]))
  (helper instr-list #f))
       

       

(define (convert-trace-to-interleavings trace library broken-methods hole)

  (define (expand-as-needed t)
    (cond
      [(empty? t) (list (list))]
      [(and (Run-method? (first t)) (list-contains broken-methods (Run-method-method (first t))))
       (all-combinations (thread-runs (Thread-list (retrieve-code library (Run-method-method (first t)))) library 1 (Run-method-ret (first t))) (expand-as-needed (rest t)))]
      [else
       (map (append-item (first t)) (expand-as-needed (rest t)))]))
       
  

  ;; Look to see if there are methods in the second trace that
  ;; need to be broken up.
  (let ([conversion-list (filter (lambda (t)
                                     (and (equal? (C-Instruction-thread-id t) 1) (Run-method? t)
                                          (list-contains broken-methods (Run-method-method t))))
                                   trace)])

    (cond
      [(empty? conversion-list)
       (list trace)]
      [else
       (let ([thread1 (filter (lambda (t)  (not (equal? (C-Instruction-thread-id t) 1))) trace)]
             [thread2-possibilities (expand-as-needed (filter (lambda (t) (equal? (C-Instruction-thread-id t) 1)) trace))])

         ;; (display "thread1: ")
         ;; (displayln thread1)
         ;; (display "CONVERSION LIST: ")
         ;; (displayln (map (lambda (thread2) (append thread1 thread2)) thread2-possibilities))
         (reduce append (map (lambda (thread2) (filter
                                                (lambda (i)
                                                  (exists-write-in-hole i hole))

                                                (interleave-lists-marker thread1 thread2)))
              thread2-possibilities)))])))


         





(define sketch-output (interleaving-to-sketch mooly-sketch-test (list "ret10" "ret20" "ret30" "throwaway0") library (Hole 0 (list) 1)))
(define all-runs (thread-runs mooly-sketch-test library 0 ""))

;; (length (reduce append (map (lambda (l) (convert-trace-to-interleavings l library (list "remove"))) all-runs)))
(displayln sketch-output)
;; ;; (displayln (interleaving-to-sketch (convert-trace-to-interleavings (first all-runs) library (list "remove"))))

