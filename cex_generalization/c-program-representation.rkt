#lang racket

(require "../program_representation/simulator-structures.rkt")
(require "../utilities/utilities.rkt")
(require "../examples/mooly-example.rkt")
(require "../examples/mooly-library.rkt")
(require racket/string)

(require "to-sketch.rkt")
(require "metasketcher.rkt")

(provide interleaving-to-sketch
         create-announcement-version
         )


(define (line-ids) (void))  

;; We keep a global counter of all the new variables in the sketch that we have created so far
;; (freshvar) gives a new program variable id
(define counter (void))
(set! counter 0)
(define (freshvar)
  (set! counter (+ counter 1))
  counter)


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


;;;;;;;;;;;;;;;;;;;;;;;;;; METHODS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Takes in a Loop C-Instruction, and returns a list of all unrollings of the loop
;; up to depth "depth". We also take in a loop-id, which is a unique id associated
;; with this particular loop.
;; Loop executions include "assume" statements which are associated with a particular id
;; The loop-id ensures that in the sketch, we can recognize all of the executions as
;; the same loop
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
     


;; Gives all possible executions of a particular list of instructions which includes branches.
;; Branches are converted to multiple traces with assume statements
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
   



;; Gives all possible executions of the loop up to depth "depth". Gives loop id "loop-id" to all
;; assume-loops, so that we can identify them with this particular loop. 
(define (all-unrolls loop depth loop-id)
  (define (all-unrolls-helper loop depth)
    ;; (display "all unrolls\n")

    (cond
      [(> 0 depth) (list)]
      [else
       (append
        (unroll-loop loop depth loop-id)
        (all-unrolls-helper loop (- depth 1)))]))

  ;; This section simply allows us to cover both "Maybe-loop"s and "Loop"s. 
  (cond
    [(Loop? loop)
       (all-unrolls-helper loop depth)]
    [(Maybe-loop? loop)
     (let ([loop-version (Loop (Maybe-loop-condition loop) (Maybe-loop-instr-list1 loop))])
       (all-unrolls-helper loop-version depth))]

    [else
     (displayln loop)
     (displayln "Need loop or maybe loop. Something is wrong")]))



       





;; Computes all possible interleavings of two instr-lists
(define (interleave-lists instr-list1 instr-list2)
  (cond
    [(empty? instr-list1) (list instr-list2)]
    [(empty? instr-list2) (list instr-list1)]
    [else
     (append
      (map (append-item (first instr-list1)) (interleave-lists (rest instr-list1) instr-list2))
      (map (append-item (first instr-list2)) (interleave-lists instr-list1 (rest instr-list2))))]))



;; Computes all possible interleavings of two instr-lists, but is aware 
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
        
        

;; Computes all interleavings of instr-list2 interrupting instr-list1 inside the hole given by
;; the argument "hole". This is specifically for unrolling possible loops, where we have a
;; trace which has been given a possible loop sketch addition. This interleaving method
;; returns the expansion of the maybe loop.
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




;; Takes a Thread-list which is a trace which may include any program element, including Sketch elements
;; Branches, Loops, Maybe-loops etc... 
;; Returns a list of concrete traces which represent all possible runs of the original trace
(define (thread-runs t library t-id to-return)

  (set! line-ids 0)


  ;; If we have a Maybe-loop, we want to expand it into all possible executions
  (define (handle-maybe-loop instr-list to-return)

    (let
        ([maybe-loop (first instr-list)]
         [possible-loops
          (reduce append (map ;; Unroll ever loop and then recursively compute thread-runs for unrolled loops
                          (lambda (l) 
                            (unroll-thread-runs (append l (rest instr-list)) to-return))
                          (all-unrolls (first instr-list) 1 (new-loop-id))))])

      (let ;; Interleave unrolled loops as necessary - only in actual holes
          ([interleavings (reduce append
                                  (map
                                   (lambda (l)
                                     (interleave-lists-at-holes l (Maybe-loop-instr-list2 maybe-loop)
                                                                (Maybe-loop-hole maybe-loop)))
                                   possible-loops))])
      interleavings)))

   
  ;; This is simply the unrolling of a particular thread -
  ;; exands into a list of traces with Assume statements representing all possible runs of
  ;; the given thread
  (define (unroll-thread-runs instr-list to-return)

    (cond
      [(empty? instr-list) ;; (display "empty\n")
       (list (list))]

      ;; Return statements simply set the return var - TODO: This may no longer make sense - check
      [(Return? (first instr-list)) ;; (display "return ") (displayln (~v to-return))
       (list (list (Set-var to-return (Return-val (first instr-list)) )))]

      ;; Single branch simply exands into the case where the branch is run, and where it's not
      [(Single-branch? (first instr-list)) ;; (display "single branch\n")
       (append
        (map (append-item (Assume-simulation (Single-branch-condition (first instr-list)))) (unroll-thread-runs (append (Single-branch-branch (first instr-list)) (rest instr-list)) to-return))
        (map (append-item (Assume-simulation (Not (Single-branch-condition (first instr-list))))) (unroll-thread-runs (rest instr-list) to-return)))]

      ;; This is a Sketch element - a possible addition to the program which either exists or does not exist
      ;; existence is determined by a unique meta-variable in the Sketch.
      ;; This is identified by the Assume-meta-condition
      [(Meta-addition? (first instr-list))
       (let ([new-meta (new-meta-var)])
         (append
          (map (append-item (Assume-meta new-meta)) (unroll-thread-runs (append (Meta-addition-instr-list (first instr-list))
                                                                                    (rest instr-list)) to-return))
          (map (append-item (Assume-not-meta new-meta)) (unroll-thread-runs (rest instr-list) to-return))))]

      ;; Like the Meta-addition, but a branch. One of the two branches is the two program,
      ;; selecting which branch is left to the synthesizer
      [(Meta-branch? (first instr-list))
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

      ;; Loops must be unrolled into every possible execution up to a certain number
      [(Loop? (first instr-list)) ;; (display "looping\n")
       
       (reduce append (map (lambda (l)  (unroll-thread-runs (append l (rest instr-list)) to-return)) (all-unrolls (first instr-list) 1 (new-loop-id))))]

      ;; This is the equivalent of the Sketch "repeat". We repeat a certain section of code some number of times.
      ;; That number is chosen by the synthesizer.
      ;; A Repeat node is associated with a particular meta-variable Repeat-meta-which-var
      ;; which is what will be synthesized in the final Sketch
      [(Repeat-meta? (first instr-list))
       (reduce append (map (lambda (l) (map (append-item (Repeat-meta l (Repeat-meta-which-var (first instr-list)))) (unroll-thread-runs (rest instr-list) to-return)))


                                (unroll-thread-runs (Repeat-meta-instr-list (first instr-list)) to-return)))]
       



      ;; This is a possible loop. Whether the loop will be included is chosen by the synthesizer -
      ;; If it is a loop, we must expand the loop and fill in the interleavings
      ;; which are equivalent to the original ones from the error trace. This is done with Handle-maybe-loop
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
    
  






;; Takes in a list of lists of C-Instructions, and returns a list of traces each with an ID   
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
(define POSSIBLE (void))
(set! POSSIBLE #t)

(define-symbolic pick-trace integer?)")



  (let ([all-runs (reduce append (map (lambda (l) (convert-trace-to-interleavings l library (list "remove") hole)) (thread-runs interleaving library 0 "")))])
        (let ([meta-vars (unique (lambda (fst snd) ;; (displayln (equal? (Tuple-a fst) (Tuple-a snd)))

 (equal? (Tuple-a fst) (Tuple-a snd))) (collect-meta-vars all-runs library))]
[new-scope (new-scope-num)])


(string-append
 prelude
 (generate-library-code library)

   (reduce string-append (map (lambda (v) (string-append "(define " v " (void))\n")) variables-of-interest))
 ;; All symbolic variables which will be referenced in the possible traces 
 (reduce string-append
         (map (lambda (v) (string-append "(define-symbolic " (Tuple-a v) " " (Tuple-b v) ")\n"))
              meta-vars))
    

 
 (trace-list-to-sketch (transform-to-traces all-runs) library "first-args" new-scope 0)

 "(synthesize #:forall (list pick-trace)
              #:guarantee (linearizable-check))"))))



    

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


         



(define (give-ids-to instr-list n)
  (cond
    [(empty? instr-list) n]
    [(Single-branch? (first instr-list))
     (set-C-Instruction-instr-id! (first instr-list) n)
     (give-ids-to
      (rest instr-list)
      (give-ids-to (Single-branch-branch (first instr-list)) (+ n 1)))]
    [else
     ;; (display (first instr-list)) (display "-") (displayln n)
     (set-C-Instruction-instr-id! (first instr-list) n)
     (give-ids-to (rest instr-list) (+ n 1))]))
    

;; (give-ids-to (Method-instr-list (list-ref library 4)) 0)

;; (define mooly-announcement-sketch (metasketch-announcement-strategy (Method-instr-list (list-ref library 4)) (Hole 5 (list) 6)))

;; (Single-branch-branch (fifth mooly-announcement-sketch))


(define sketch-output (interleaving-to-sketch mooly-sketch-test (list "ret1" "ret2" "ret3" "throwaway") library (Hole 0 (list) 1)))
(define all-runs (thread-runs mooly-sketch-test library 0 ""))

;; (length (reduce append (map (lambda (l) (convert-trace-to-interleavings l library (list "remove"))) all-runs)))
;; (displayln sketch-output)
;; ;; (displayln (interleaving-to-sketch (convert-trace-to-interleavings (first all-runs) library (list "remove"))))

