#lang rosette
(require rosette/lib/synthax)
(require racket/dict)
(require racket/match)
(require racket/string)
(require rosette/lib/angelic)
(require "../program_representation/concurrent-object-lib.rkt")
(require "../program_representation/simulator-structures.rkt")
(require racket/trace)





;; Example client
(define client-pre-example
  (Client-pre
   (list
    (Thread-list
     (list
      (Client-pre (list (Instruction (lambda (env) (list-add env "l1" 1)) #f 0 0 0 #f))) ;; TODO option not to return
      (Client-pre (list (Sketch-placeholder "newMethod"))))))))

       
;; Example method
(define method-example
  (Method "newMethod"
   (list (Instruction (lambda (env) (list-add env "l1" 12 )) #t 1 0 0 #f)
    (Instruction (lambda (env) (list-remove env "l1" 0 "z1")) #t 1 0 0 #f))))


;; Helper function to insert a list into the middle of another list
(define (replace-with-list l a l-new)

  (if (empty? l)
      `()

         
      (if (equal? a (first l))
          
          (append l-new (rest l))
          (append (list (first l)) (replace-with-list (rest l) a l-new)))))


(define (replace-all-sketches l l-new)
  (if (empty? l)
      `()
      (match (first l)
        [(Sketch-placeholder n) (append l-new (rest l))]
        [_ (append (list (first l)) (replace-all-sketches (rest l) l-new))])))

;; Process Client-pre object to replace all calls to newMethod
(define (replace-calls c m)
  (let ([replace-calls-m
         (lambda (x)
           (replace-calls x m))])
    (let ([recurse-threads
           (lambda (elem)

             (match elem
               [(Thread-list i-list) (Thread-list (map replace-calls-m i-list))]
               [_ elem]))])

      (match m
        [(Method m-name m-list)
         (match c        
           [(Client-pre instr-list)
            (let ([processed-instr-list (map recurse-threads instr-list)])

              (Client-pre (replace-all-sketches processed-instr-list m-list)))]
           [_ (display "Error: replace-calls can only take a Client-pre\n")])]
        [_ (display "Error: replace-calls must take a Method\n")]))))
    

(define ret (replace-calls client-pre-example method-example))
;; (Client-pre-instr-list (first  (Thread-list-instr-list (first (Client-pre-instr-list ret)))))
;; (define oldRet (Client-pre-instr-list client-pre-example))

;; (Client-pre-instr-list (first (rest (Thread-list-instr-list (first (Client-pre-instr-list ret))))))

;; Create new Client-post object with all possible interleavings of threads
;; TODO: Note that this only allows one level of recursion. You cannot have threads with other inner threads

(define-struct Client-post (instr-list) #:transparent)
(define-struct Thread-interleavings (instr-list))

(define (find-operation-with-id h-list id)
  (define (found-id? x)
    (equal? (Operation-id x) id))
  (first (filter found-id? h-list)))

(define (happens-before h-list instr1 instr2)
  (cond
    [(Assume? instr1) #t]
    [(Assume? instr2) #t]
    [else 
     (let ([id1 (Instruction-id instr1)] [id2 (Instruction-id instr2)])
       (let ([op1 (find-operation-with-id h-list id1)] [op2 (find-operation-with-id h-list id2)])
         (let ([interval1 (Operation-interval op1)] [interval2 (Operation-interval op2)])
           ;; (display (> (Interval-start interval2) (Interval-end interval1)))
           ;; (display "\n")
           (> (Interval-start interval2) (Interval-end interval1)))))]))
    ;; (let ([interval1 (Operation-interval op1)] [interval2 (Operation-interval op2)])
    ;;   (> (Interval-start op2) (Interval-end op1))))
    


(define (all-list-combos set1 set2)
  (define (append-list l1)
    (lambda (l2) (append l1 l2)))
  (cond
    [(empty? set1) (list (list))]
    [(empty? set2) (list (list))]
    [else
     (append
      (map (append-list (first set1)) set2)
      (all-list-combos (rest set1) set2))]))

;; Unpacks all possible branches of a list of instructions into separate traces
(define tracker (void))
(set! tracker 0)
(define (unpack-branches l)
  (set! tracker (+ tracker 1))
  ;; (display "Unpack called: ") (display tracker) (display " times - length: ") (display (length l)) (display "\n")
  (define (append-list l)
    (lambda (l2)
      (append l l2)))
  (cond
    [(empty? l) (list (list))]
    [(Loop? (first l))   (unpack-branches (unroll-loop (first l) 1))] ;; TODO - have to come up with a real solution for this
    [(Assume? (first l)) (map (append-item (first l)) (unpack-branches (rest l)))]
    [(Branch? (first l)) ;; (display (unpack-branches (Branch-branch1 (first l)))) (display "\n") (display (unpack-branches (Branch-branch2 (first l)))) (display "\n\n\n")
     (append
      (all-list-combos (filter (lambda (l) (not (empty? l))) (unpack-branches (Branch-branch1 (first l)))) (unpack-branches (rest l)))
      (all-list-combos (filter (lambda (l) (not (empty? l))) (unpack-branches (Branch-branch2 (first l)))) (unpack-branches (rest l))))]
      ;; (map (append-list (Branch-branch2 (first l))) (unpack-branches (rest l))))]
    [else
     (map (append-item (first l)) (unpack-branches (rest l)))]))
    
  
(define (atomic? instr)
  (not (= (Instruction-atomic instr) 0)))



(define (is-possible-execution? l)
  
  (cond
    [(> 2 (length l)) 9 #t]
    [(or (Assume? (first l)) (Assume? (second l))) #t] ;; TODO - not sure what needs to go here
    [else (and
           (not (and (atomic? (first l)) (not (equal? (Instruction-id (second l)) (Instruction-id (first l))))))
           (is-possible-execution? (rest l)))]))
    ;; [(and (atomic? (second l)) (not Instruction-is-method (first l))) (display "false\n")
    ;;  #f]
    ;; [else (display (atomic? (first l))) (display "-") (display (Instruction-id (second l))) (display "-") (display (Instruction-id (first l))) (display "\n\n") (is-possible-execution? (rest l))]))



                  
(define (unroll-loop loop depth)

  (cond
    [(equal? depth 0) (list (Assume (lambda (e) (not ((Loop-condition loop) e)))))]
    [else
     (append (list (Assume (Loop-condition loop)))  (Loop-instr-list loop) (unroll-loop loop (- depth 1)))]))



(define (range n)
  (if (= n 0)
      (list 0)
      (append (list n) (range (- n 1)))))

(define (all-unrolls loop depth)
  (define (unroll-loop-with-arg n)
    (unroll-loop loop n))
  (map unroll-loop-with-arg (range depth)))

(define (history-aware-shuffles l1 l2 h-list)
  (define (append-list l)
    (lambda (l2)
      (append l l2)))
  (display "l1: ") (display l1) (display "\n")
  (display "l2: ") (display l2) (display "\n")
  (display "h-list: ") (display h-list) (display "\n")
  (cond
    [(empty? l1) (unpack-branches l2)] ;; TODO - the problem is (rest l1) doesn't unpack all the branches!
    [(empty? l2) (display "l2 empty, unpacking l1: ") (display (length (unpack-branches l1))) (display "\n")(unpack-branches l1)]
    [(Loop? (first l1))
     (append

      ;; (history-aware-shuffles (append (unroll-loop (first l1) 2) (rest l1)) l2 h-list)

      ;; (history-aware-shuffles (append (unroll-loop (first l1) 1) (rest l1)) l2 h-list)
      (history-aware-shuffles (append (unroll-loop (first l1) 0) (rest l1)) l2 h-list))]

    [(Loop? (first l2)) (history-aware-shuffles l1 (append (list (all-unrolls (first l2) 3)) (rest l2)) h-list)]
    ;;   (history-aware-shuffles (append (unroll-loop  (first l1) 3) (rest l1)) l2 h-list)]
      
    [(Branch? (first l1)) ;; TODO - There are no assume statements.
     (append
       (history-aware-shuffles (append (Branch-branch1 (first l1)) (rest l1)) l2 h-list)
       (history-aware-shuffles (append (Branch-branch2 (first l1)) (rest l1)) l2 h-list))]
    [(Branch? (first l2))
     (append
      (history-aware-shuffles l1 (append (Branch-branch1 (first l2)) (rest l2)) h-list)
      (history-aware-shuffles l1 (append (Branch-branch2 (first l2)) (rest l2)) h-list))]
    ;; [(atomic? (first l1))
    ;;  (map (append-item (first l1)) (history-aware-shuffles (rest l1) l2 h-list))]
    ;; [(atomic? (first l2))
    ;;  (map (append-item (first l2)) (history-aware-shuffles l1 (rest l2) h-list))]
    [(happens-before h-list (first l1) (first l2)) ;; (display "happens before (first l1) (first l2)\n")
     (map (append-item (first l1)) (history-aware-shuffles (rest l1) l2 h-list))]
    [(happens-before h-list (first l2) (first l1)) ;; (display "happens before (first l2) (first l1)\n")
     (map (append-item (first l2)) (history-aware-shuffles l1 (rest l2) h-list))]
    [else ;; (display "no happens before\n")
          (let ([all-shuffles2 (map (append-item (first l2)) (history-aware-shuffles l1 (rest l2) h-list))]
                [all-shuffles1 (map (append-item (first l1)) (history-aware-shuffles (rest l1) l2 h-list))])
            ;; (display "all-shuffles: " ) (display (append all-shuffles1 all-shuffles2)) (display "\n")
            (append all-shuffles1 all-shuffles2))]))


     



     
(define (shuffles l1 l2)
  ;; (display "shuffling - ") (display l1) (display "-") (display l2) (display "\n")
  
  (let ([add-to-all 
           (lambda (elem)
             (lambda (tail) (append (list elem) tail)))])
    (if (empty? l1)
        (list l2)
        (if (empty? l2)
            (list l1)
            
            (append 
             (map (add-to-all (first l1)) (shuffles (rest l1) l2))
             (map (add-to-all (first l2)) (shuffles l1 (rest l2))))))))
                      
;; Takes a set of threads and shuffles them into possible interleavings
;; TODO: For now assumes just 2 threads
(define (create-threads t h-list)

  (match t
    [(Thread-list list-set)
     (let ([first-c (Client-pre-instr-list (first list-set))] [second-c (Client-pre-instr-list (first (rest list-set)))])
       ;; (display "so far so good\n")
       (let ([all-shuffles (history-aware-shuffles first-c second-c h-list)])
         ;; (display (length all-shuffles)) (display "\n")
         (Thread-interleavings all-shuffles)))]
    [_ t]))




(define (generate-interleavings c h-list)
  (define (create-threads-h t)
    (create-threads t h-list))
         
  (match c
    [ (Client-pre instr-list)
      (Client-post (map create-threads-h instr-list))]
    [ _ (display "Error: Need a Client-pre object\n")]))

(define (get-interleavings-as-lists c)
  (match c
    [(Client-post instr-list)
     (define (add-tail l)
       (append l (rest instr-list)))

     (if (empty? instr-list)
         `()
         (if (Thread-interleavings? (first instr-list))
             (map add-tail (Thread-interleavings-instr-list (first instr-list)))

             
             (append (first instr-list) (get-interleavings-as-lists (rest instr-list)))
             ))]))


(define-struct Instruction-sequence (possible? id seq))



(define (print-instruction-sequence s)
  (define (print-instr instr)
    (cond
      [(Instruction? instr) (display (Instruction-id instr))]
      [(Assume? instr) (display "assumption")])

    (display "\n"))
  (for-each print-instr (Instruction-sequence-seq s)))


(define (print-history-op-list h-list)
  (define (print-op op)
    (display (Operation-id op))
    (display "\n"))
  (for-each print-op h-list))


;; Assigns a unique id to any program interleaving 
(define (calculate-sequence-id seq)
  ;; (display seq) (display "\n")
  (cond
    [(empty? seq) `()]
    [(or (Continue? (first seq)) (Assume? (first seq)) (Branch? (first seq))) (calculate-sequence-id (rest seq))]
    [else (append (list (Instruction-inner-id (first seq))) (calculate-sequence-id (rest seq)))]))


(define (add-ids-to-instruction-lists inst-seq-list)

  (define (add-id inst-list i)
    (if (empty? inst-list)
        `()
        (append (list (Instruction-sequence
                       (is-possible-execution? (first inst-list))
                       (calculate-sequence-id (first inst-list))
                       (first inst-list)))
                (add-id (rest inst-list) (+ 1 i)))))
  (add-id inst-seq-list 0))
    


;; (define (run-lists list-set env)
;;   (define (run-list e)
;;     (lambda (l)
;;       (if (empty? l)
;;           e
;;           ((run-list ((Instruction-i  (first l)) e)) (rest l)))))

;;   (let ([instr-seq (map Instruction-sequence-seq list-set)])
;;     (map (run-list env) instr-seq)))

(define (run-lists list-set env)
  ;; (display "Running lists...\n")

  (define (run-list l e id possible?)

    (cond
      [(empty? l) (add-entry (add-entry e "possible" possible?) "id" id)]
      [(Assume? (first l)) (if ((Assume-condition (first l)) e)
                               (run-list (rest l) e id possible?)
                               (add-entry (add-entry e "possible" #f) "id" id))]
                            
      [else (run-list (rest l) ((Instruction-i (first l)) e) id possible?)]))
  
  
  ;; (define (run-list l e id possible?)
  ;;   (case
  ;;    [(empty? l) (add-entry (add-entry e "possible"  possible?) "id" id)]


  ;;    [else (run-list (rest l) ((Instruction-i (first l)) e) id possible?)]))  ;; ((Instruction-i  (first l)) e)) (rest l) )))
    ;; (cond
    ;;   [(empty? l) (add-entry e "id" id)]
    ;;   [((run-list ((Instruction-i (first l)) e)) (rest l))]))
  
  (cond
    [(empty? list-set) (list)]
    [else 
     (let
         ([instr-seq (Instruction-sequence-seq (first list-set))]
          [instr-seq-id (Instruction-sequence-id (first list-set))]
          [instr-seq-possible (Instruction-sequence-possible? (first list-set))])
       (append
        (list (run-list instr-seq env instr-seq-id instr-seq-possible))
        (run-lists (rest list-set) env)))]))
       



;; (define ret (replace-calls client-pre-example method-example))


;; (run-lists (add-ids-to-instruction-lists (get-interleavings-as-lists (generate-interleavings ret))) (list (list "l1" 1 2 3)))
;; (add-ids-to-instruction-lists (get-interleavings-as-lists (generate-interleavings ret)))




;; Generates a set of Instruction sequences that can be run on an environement from a Client-pre and a method sketch
(define (generate-simulation client method-sketch history)
  ;; (display (Instruction-sequence-id (third (add-ids-to-instruction-lists (get-interleavings-as-lists (generate-interleavings (replace-calls client method-sketch) (History-op-list history)))))))
  ;; (display "\n") 
  ;; (display (is-possible-execution? (Instruction-sequence-seq (third (add-ids-to-instruction-lists (get-interleavings-as-lists (generate-interleavings (replace-calls client method-sketch) (History-op-list history))))))))
  ;; (display "\n____________\n")
  (add-ids-to-instruction-lists (get-interleavings-as-lists (generate-interleavings (replace-calls client method-sketch) (History-op-list history)))))



;; (generate-simulation client-pre-example method-example)

;; We need a way to represent histories
(define-struct History (start-env op-list))
(define-struct Operation (op interval id))
(define-struct Interval (start end))


(define (append-item item)
  (lambda (l) (append (list item) l)))


;; Get all sequential runs represented by a history
(define (get-seq-runs h)

  (define (extract-seq-runs o-list)
    (if (and (not (empty? o-list)) (> (length o-list) 1))
        (let ([first-o (first o-list)] [second-o (first (rest o-list))])
          (let ([first-i (Operation-interval first-o)] [second-i (Operation-interval second-o)])
              
            (cond
              [(<= (Interval-start second-i) (Interval-end first-i))
               (append

                (map (append-item (Instruction (Operation-op first-o) #f (Operation-id first-o) 0 0)) (extract-seq-runs (rest o-list)))
                (map (append-item (Instruction (Operation-op second-o) #f (Operation-id second-o) 0 0)) (extract-seq-runs (append (list first-o) (rest (rest o-list))))))] 
              [else (map (append-item (Instruction (Operation-op first-o) #f (Operation-id first-o) 0 0)) (extract-seq-runs (rest o-list)))])))
        
        (cond
          [(empty? o-list) (list (list))]
          [else (list (list (Instruction (Operation-op (first o-list)) #f (Operation-id (first o-list)) 0 0)))])))
       

  
  (match h
    [(History start-env op-list) (extract-seq-runs op-list)]
    [_ (display "Error: Need a valid history for sequential runs\n")]))

(define test-hist
  (History (list (list "l1" 1 2 3) (list "z0"  5) )
           (list
            (Operation (lambda (env) (list-add env "l1" "z0")) (Interval 0 2) 0)
            (Operation (lambda (env) (list-remove env "l1" 3 "z1")) (Interval 2 3) 1))))


(define (run-lists-no-seq list-set env)
  (define (run-list e)
    ;; (display e)
    ;; (display "\n")

    (lambda (l)
      (if (empty? l)
          e
          ((run-list ((Instruction-i (first l)) e)) (rest l)))))


    (map (run-list env) list-set))


;; (define (run-operation-lists h-list env)
;;   (define (run-op-list e)
;;     (lambda (l)
;;       (if (empty? l)
;;           e
;;           ((run-op-list ((Operation-op (first l)) e) (rest l))))))
;;   (map (run-op-list env) h-list))
  


(define (interested-vals env-list)
  (cond
    [(empty? env-list) (list (list))]
    [else
     (append (list (list (cons "r0" (get-mapped (first env-list) "r0")) (cons "z0" (get-mapped (first env-list) "z0")) (cons "possible" (get-mapped (first env-list) "possible")))) ;; (cons "id" (get-mapped (first env-list) "id"))))
             (interested-vals (rest env-list)))]))



;; Generate valid environments of a history
(define (valid-environments h)
  (let ([seq-runs (get-seq-runs h)]) ;; TODO: Why is this surrounded with an extra layer of list...
    (interested-vals (run-lists-no-seq seq-runs (History-start-env h)))))


;; (define valid-envs-list (valid-environments test-hist))

;; Returns if the interval of i2 intersects with i1
(define (interrupted i1 i2)
  (<= (Interval-start i2) (Interval-end i1)))

;; Removes the operation matching the given instruction in a history operation list
(define (remove-matching-op instr h-list)
  ;; (display (equal? (Instruction-id instr) (Operation-id (first h-list)))) (display "\n")
  ;; (display "instr: ") (display  (Instruction-id instr)) (display "-hfirst: ") (display (Operation-id (first h-list))) (display "\n")
  (if (empty? h-list)
      `()
      (cond
        [(equal? (Instruction-id instr) (Operation-id (first h-list)))
         (rest h-list)]
        [else (append (list (first h-list)) (remove-matching-op instr (rest h-list)))])))



(define (reacheable instr h-list)
  (if (> (length h-list) 1)
      (let ([first-o (first h-list)] [second-i (Operation-interval (first (rest h-list)))]
            [first-i (Operation-interval (first h-list))])
        (if (equal? (Instruction-id instr) (Operation-id (first h-list)))
            #t
            (cond
              [(interrupted first-i second-i) (reacheable instr (rest h-list))]
              [else #f])))
      (cond
        [(empty? h-list) #f]
        [else (equal? (Instruction-id instr) (Operation-id (first h-list)))])))



;; Takes a list of Instructions representing a sketched method that may have been interrupted
;; along with a history operation list. Returns whether this interrupted method is consistent
;; with the history operation list
;; Returns: a tuple containing (#f, `() ) if m does not satisfy h-list, or (#t, h-list') where
;; h-list' is h-list with all the operations in m removed
(define (method-implements-history m h-list id)
  (cond
    [(empty? m) (cons #t h-list)]
    [(equal? (Instruction-id (first m)) id)
     (if (reacheable (first m) h-list)
         (method-implements-history (rest m) h-list id)
         (cons #f `()))]
     [else
      (if (reacheable (first m) h-list)
          (method-implements-history (rest m) (remove-matching-op (first m) h-list) id)
          (cons #f `()))]))
                     
      

(define (get-method-with-interruptions t id)
  (define-struct Tuple (a b))
  (define (get-method-helper t id) ;; we need a flag to tell us that we've found the final statement in the method
   
      (if (empty? t)
          (Tuple #f `())

          (let ([recursed-sol (get-method-helper (rest t) id)])
            (let ([found (Tuple-a recursed-sol)] [solution (Tuple-b recursed-sol)])
              ;; (display "found: ") (display found) (display "\n")
              ;; (display "solution: ") (display solution) (display "\n")
              (cond
                [found (Tuple #t (append (list (first t)) solution))]
                [(equal? id (Instruction-id (first t)))                 
                 (Tuple #t (list (first t)))]
                [else (Tuple #f `())])))))
                
       
       
      ;; [(equal? id (Instruction-id (first t)))
       
  (Tuple-b (get-method-helper t id)))
      
      

    
      
;; Once we have finished checking a method, we must remove all the method instructions and
;; interruptions from a trace
(define (remove-method-from-trace t m)
  (define (from b l)
    (if (empty? l)
        `()
        (if (> b 0)
            (from (- b 1) (rest l))
            l)))
  (from (length m) t))


(define (remove-op-id h-list id)
  (define (is-not-id? op)
    (not (equal? id (Operation-id op))))
  (filter is-not-id? h-list))


;; Checks whether a given execution trace is an instance of a given history operation list
(define (implements-history t h-list)

  ;; (display "length: ") (display (length h-list) ) (display "\n")
  ;; (display "length of t: " ) (display (length t) ) (display "\n")
  ;; (print-history-op-list h-list)
  ;; (display (Instruction-id (first t)))



  (if (not (empty? t))
      (cond    
        [(Instruction-is-method (first t))
         (let ([m (get-method-with-interruptions t (Instruction-id (first t)))])
           (let ([ret (method-implements-history m h-list (Instruction-id (first t)))])
             (let ([doable (first ret)] [new-h-list (rest ret)])
               (if doable
                   (implements-history (remove-method-from-trace t m) (remove-op-id new-h-list (Instruction-id (first t))))
                   #f))))]
                   
        [else
         (let ([is-reacheable (reacheable (first t) h-list)])
           ;; (display is-reacheable)
           ;; (display "-") (display (remove-matching-op (first t) h-list)) (display "-") (display t)
           ;; (display "\n")
           (if (not is-reacheable)
               #f
               (implements-history (rest t) (remove-matching-op (first t) h-list))))])
      (cond
        [(empty? h-list) #t]
        [else  #f])))
         
  
           
     
  ;; (reacheable (first (rest t)) h-list))






(define not-an-instance
  (list
   (Instruction `() #f 0 0 0 #f)
   (Instruction `() #f 1 0 0 #f)
   (Instruction `() #f 2 0 0 #f)))

(define hist-op-list
  (list
   (Operation `() (Interval 0 7) 0)
   (Operation `() (Interval 6 10) 2)
   (Operation `() (Interval 11 15) 1)))

;; (implements-history not-an-instance hist-op-list)
   



;; Returns all instruction list id's that satisfy a history
(define (all-that-implement i-list h)
  (if (empty? i-list)
      `()
      (let ([h-list (History-op-list h)])
        (let ([implements? (implements-history (Instruction-sequence-seq (first i-list)) h-list)])
          (if implements? 
              (append (list (Instruction-sequence-id (first i-list))) (all-that-implement (rest i-list) h))
              (all-that-implement (rest i-list) h))))))
          

(define-symbolic u1 integer?)
(define-symbolic u2 integer?)
(define-symbolic u3 integer?)
(define env (list (list "l1" u1 u2 u3) (cons "z5" 10) (cons "z0" 5) (cons "z1" 6)))
;; Example sketch!
(define x (??))
(define y (??))
(define example-sketch
  (Method "newMethod"
          (list
           (Instruction (lambda (e)
                          (cond
                            [(= x 0) (list-add e "l1" "z0")]
                            [(= x 1) (list-add e "l1" "z1")]
                            [(= x 2) (list-remove e "l1" 0 "z6")])) #t 0 0 0 #f)

           (Instruction (lambda (e)
                          (cond
                            [(= y 0) (list-add e "l1" "z0")]
                            [(= y 1) (list-add e "l1" "z1")]
                            [(= y 2) (list-remove e "l1" 0 "z7")])) #t 0 0 0 #f))))

(define example-client
  (Client-pre
   (list
    (Thread-list
     (list
      (Client-pre
       (list
        (Sketch-placeholder "newMethod")))
      (Client-pre
       (list
        (Instruction (lambda (e) (list-add e "l1" "z5")) #f 1 0 0 #f)
        (Instruction (lambda (e) (list-remove e "l1" 4 "z6")) #f 2 0 0 #f))))))))


(define (newMethod e)
  (let ([newE (list-add e "l1" "z0")])
    (list-remove newE "l1" 0 "z7")))

(define example-history
  (History env
   (list
   (Operation
    (lambda (e) (list-add e "l1" "z5")) (Interval 0 3) 1)
   (Operation (lambda (e) (newMethod e)) (Interval 2 5) 0)
   (Operation (lambda (e) (list-remove e "l1" 4 "z6")) (Interval 6 11) 2))))



;; TODO: verify that this only generates traces that are actually executable with the client
;; (define (all-interleavings-from-history client h-list)
;;   (if (empty? h-list)
;;       (list `())
;;       (let ([first-o (first h-list)] [second-i (Operation-interval (first (rest h-list)))]
;;             [first-i (Operation-interval (first h-list))])

;;         (cond
;;           ;; If we are happening at the same time as another operation, get all possible interleavings of all
;;           ;; reacheable statements
;;           [(exist-reacheable h-list) 
;;           [(
      
      





;; (define valid-envs (valid-environments example-history))
;; valid-envs
;; (define sim (generate-simulation example-client example-sketch example-history))
;; (define all-valid (valid-environments example-history))

(define (list-includes l item)
  ;; (display "matches: ")(display (filter (lambda (x) (or (equal? x item) (equal? item "id"))) l)) (display "\n")
  ;; (display "item: ") (display item) (display "\n")
  (< 0 (length (filter (lambda (x) (or (equal? x item) (equal? item "id"))) l))))

(define (list-set-contains l1 l2)
  (if (empty? l1)
      #t
      (and (list-includes l2 (first l1)) (list-set-contains (rest l1) l2))))

(define (list-set-equivalent l1 l2)
  (and (list-set-contains l1 l2) (list-set-contains l2 l1)))
  

(define (get-keys m)
  (map car m))

(define (return-vals-equivalent keys l1 l2)
  (define (equal-if-ret? key a b)
    ;; (display "Checking equality: key - ") (display key) (display "- a: ") (display a) (display " - b: ") (display b) (display "\n")
    ;; (display "result-equality: ") (display (equal? a b)) (display "\n")
    (cond
      [(equal? key "z0") (equal? a b)]
      [(equal? key "r0") (equal? a b)]
      [else #t]))
    ;; (cond
    ;;   [(list? a) (list? b)]
    ;;   [(equal? key "id") #t]
    ;;   [else #t]))
  (if (empty? keys)
      #t
      (and (equal-if-ret? (first keys) (get-mapped l1 (first keys)) (get-mapped l2 (first keys)))
           (return-vals-equivalent (rest keys) l1 l2))))

;; NOTE: For now equivalence only considers return values
(define (equivalent-envs? env1 env2)
  ;; (display "equivalent? - ")
  ;; (display env1) (display "\n")
  ;; (display env2) (display "\n")
  (let ([keys1 (get-keys env1)] [keys2 (get-keys env2)])
    ;; (display keys1) (display " - ") (display keys2) (display ":::: ") (display (list-set-equivalent keys1 keys2)) (display "\n")
    (return-vals-equivalent keys1 env1 env2)))
    ;; (and
    ;;  (list-set-equivalent keys1 keys2)
    ;;  (return-vals-equivalent keys1 env1 env2))))

;; (all-that-implement sim example-history)
;; (define keys (get-keys (first all-valid)))



(define (has-equivalent-env env env-list)
  (< 0 (length (filter (lambda (e) (equivalent-envs? env e)) env-list))))

(define (generate-invariant-assertions client sketch history sim)
  ;; (display "generating invariants\n")
  (let
       ([history-correct-envs (valid-environments history)])
    ;; (display "have correct history envs\n")
    ;; (let
    ;;     ([implement-history-list (all-that-implement sim history)])
      ;; (display "Have who implements the histories\n")
      ;; (display implement-history-list) (display "\n") ;; TODO THIS IS TEMPORARY

      (lambda (env)
        ;; (display "GENERATING ASSERTIONS\n")
        (define (create-assertion-for id)
          ;; (display "Asserting ") (display (get-mapped env "id"))  (display "=>\n") (display env) (display "\n==") (display history-correct-envs) (display "\n Result: ") (display  (has-equivalent-env env history-correct-envs)) (display "\n")

          ;; (display "Asserting: ") (display (is-possible-env env)) (display "=>\n") (display env) (display "\n==") (display history-correct-envs) (display "\n Result: ") (display (has-equivalent-env env history-correct-envs)) (display "\n")
          ;; (display "asserting something\n") (display (or (not (is-possible-env env))) (has-equivalent-env env history-correct-envs)) (display "\n")

          (let ([check-var  (or (not (is-possible-env env)) (has-equivalent-env env history-correct-envs))])
            ;; (display "-------") (display check-var) (display "-----------\n")
            (assert check-var)))


          ;; (assert (or (not (is-possible-env env)) (has-equivalent-env env history-correct-envs))))
          
          
        (for-each create-assertion-for sim))))
        ;; (for-each create-assertion-for implement-history-list)))))
          
        


;; (define (verify-sketch-with-history client sketch history)
;;   (let ([sim (generate-simulation client sketch history)])
;;     (display "Finished generating sim\n")
;;     (let ([all-asserts (generate-invariant-assertions client sketch history sim)])
;;       (display "finished all asserts\n")
;;       (for-each
;;        all-asserts
;;        (run-lists sim (History-start-env history))))))

(define (is-possible-env env)
  (if (empty? env)
      #t
      (cdr (third env))))

(define (verify-sketch-with-history sim history-correct-envs generated-envs client sketch history)
  (let ([all-asserts (generate-invariant-assertions client sketch history sim)])
    ;; (display "finished all asserts\n")
    ;; (display (length generated-envs)) (display "\n")
    (for-each
     all-asserts
     generated-envs)))



(define (find-counter-example sim env-list history-correct-envs)
  (define (get-first-counter-example env-list history-correct-envs)
    (cond
      [(empty? env-list) `()]
      [else
       (if (has-equivalent-env (first env-list) history-correct-envs)
        (get-first-counter-example (rest env-list) history-correct-envs)
        (get-mapped (first env-list) "id"))]))
        
  (get-first-counter-example env-list history-correct-envs))


;; (define (find-counter-example client sketch history)
;;   (define (get-first-counter-example env-list history-correct-envs)
;;     (cond
;;       [(empty? env-list) `()]
;;       [else
;;        (if (has-equivalent-env (first env-list) history-correct-envs)
;;         (get-first-counter-example (rest env-list) history-correct-envs)
;;         (get-mapped (first env-list) "id"))]))
        
;;   (let ([sim (generate-simulation client sketch history)])
;;     (let ([env-list (run-lists sim (History-start-env history))])
;;       (let ([history-correct-envs (valid-environments history)])
;;         (get-first-counter-example env-list history-correct-envs)))))


;; (define binding
;;     (synthesize #:forall (list u1 u2 u3)
;;                 #:guarantee (verify-sketch-with-history example-client example-sketch example-history)))
;; binding

;; (define ex-sim (generate-simulation example-client example-sketch example-history))


;; (define (amap-get e m-name key target-var)
;; (define (amap-remove e m-name key target-var)
;; (define (amap-put e m-name key val)
;; (define (amap-putIfAbsent e m-name key val target-var)
;; (define (amap-contains e m-name key target-var)




;; Program that's correct in seq but broken in conc
(define (reference e)
  (let
      ([e1 (update-mapped e "val" "Null")])
    (let ([e2 (amap-contains-arg e1 "m1" "a1" "found")])
      (if (get-mapped e2 "a1")
          (let ([e3 (amap-get-arg e2 "m1" "a1" "val")])
            (let ([e4 (amap-remove-arg e3 "m1" "a1" "forget")])
              (update-mapped e4 "r0" (get-mapped e4 "val"))))
          (update-mapped e2 "r0" (get-mapped e2 "val"))))))

(define conc-env (list (list "m1" (AMap-entry "A" 12)) (cons "a1" "A")))

;; (display (amap-get-arg conc-env "m1" "a1" "val"))
;; (display "\n")



;; (i is-method id atomic)
(define (set-maybe-atomic instr-list)
  (cond
    [(empty? instr-list) `()]
    [(Branch? (first instr-list))
     (append
      (list (Branch (Branch-condition (first instr-list))
                    (set-maybe-atomic (Branch-branch1 (first instr-list)))
                    (set-maybe-atomic (Branch-branch2 (first instr-list)))))
      (set-maybe-atomic (rest instr-list)))]
    
    [else
     (define-symbolic* mystery-const integer?)
     (display "creating new constant...\n")
     (assert (= mystery-const 1))
     (append
      (list (Instruction (Instruction-i (first instr-list)) (Instruction-is-method (first instr-list))
                         (Instruction-id (first instr-list)) mystery-const (Instruction-inner-id (first instr-list)) )) ;; Should be (??)
      (set-maybe-atomic (rest instr-list)))]))


(define (retrieve-maybe-atomics instr-list)
  (cond
    [(empty? instr-list) `()]
    [(Branch? (first instr-list))
     (append
      (retrieve-maybe-atomics (Branch-branch1 (first instr-list)))
      (retrieve-maybe-atomics (Branch-branch2 (first instr-list)))
     (retrieve-maybe-atomics (rest instr-list)))]
    [else
     (append (list (Instruction-atomic (first instr-list))) (retrieve-maybe-atomics (rest instr-list)))]))


;; (define-symbolic* mystery integer?)
;; (define-symbolic* mystery2 integer?)
;; (define-symbolic* mystery3 integer?)
;; (define-symbolic* mystery4 integer?)
;; (define-symbolic* mystery5 integer?)
(define mystery 1)
(define mystery2 1)
(define mystery3 0)
(define mystery4 0)
(define mystery5 0)

(define mystbool 0)
;; (define-symbolic* mystbool  integer?)
;; (assert (= mystbool 0))

(assert (or (= mystery2 0) (= mystery2 1)))

(assert (or (= mystery 0) (= mystery 1)))
(assert (or (= mystery3 0) (= mystery3 1)))
(assert (or (= mystery4 0) (= mystery4 1)))
(assert (or (= mystery5 0) (= mystery5 1)))




(define concurrent-client
  (Client-pre
   (list
    (Thread-list
     (list
      (Client-pre
       (list
        (Sketch-placeholder "removeAttribute")))
      (Client-pre
       (list
        (Instruction (lambda (e) (amap-remove-arg e "m1" "a1" "z0")) #f 0 0 0 #f))))))))
       

(define concurrent-history
  (History conc-env
           (list
            (Operation (lambda (e) (amap-remove-arg e "m1" "a1" "z0")) (Interval 0 2) 0)
            (Operation (lambda (e) (reference e)) (Interval 1 3) 1))))



;; (define fixable-concurrent-sketch
;;   (Method (Method-id concurrent-sketch) (set-maybe-atomic (Method-instr-list concurrent-sketch))))


;; (define conc-sim (generate-simulation concurrent-client concurrent-sketch concurrent-history))






(define (counter-example-atomic-fix cex sketch)
  (define (find-interrupted l)
    (cond
      [(< (length l) 3) -1]
      [(and (>= (first l) 100) (< (second l) 100) (>= (third l) 100))
       (third l)]
      [else (find-interrupted (rest l))]))

  (define (add-atomic-to-id id instr-list)
    (cond
      [(empty? instr-list) `()]
      [(Branch? (first instr-list))
       (append           
        (list (Branch (Branch-condition (first instr-list))
                (add-atomic-to-id id (Branch-branch1 (first instr-list)))
                (add-atomic-to-id id (Branch-branch2 (first instr-list)))))
        (add-atomic-to-id id (rest instr-list)))]
      [  (= (Instruction-inner-id (first instr-list)) id)
       (define-symbolic* m integer?)
       (assert (or (= m 1) (= m 0)))
       (append
        (list
         (Instruction (Instruction-i (first instr-list)) (Instruction-is-method (first instr-list))
                      (Instruction-id (first instr-list)) m (Instruction-inner-id (first instr-list))))
        (add-atomic-to-id id (rest instr-list)))]
      [else (append (list (first instr-list)) (add-atomic-to-id id (rest instr-list)))]))
                
        
  
  (Method (Method-id sketch) (add-atomic-to-id (find-interrupted cex) (Method-instr-list sketch))))





;; (define cex (find-counter-example concurrent-client concurrent-sketch concurrent-history))  


;; (define maybe-fixed (counter-example-atomic-fix cex concurrent-sketch))


;; (find-counter-example concurrent-client (Method "fixed" maybe-fixed) concurrent-history)

;; (run-lists conc-sim (History-start-env concurrent-history))

;; (define binding
;;   (synthesize #:forall (list)
;;               #:guarantee (verify-sketch-with-history concurrent-client (Method "fixed" maybe-fixed) concurrent-history)))
;; binding



;;   (let ([sim (generate-simulation client sketch history)])
;;     (let ([env-list (run-lists sim (History-start-env history))])
;;       (let ([history-correct-envs (valid-environments history)])

(define concurrent-sketch
  (Method "removeAttribute"
          (list
           (Instruction (lambda (e) (display "line: 1\n")(update-mapped e "val" "Null")) #t 1 mystery4 100 #f)
           (Instruction (lambda (e) (display "line:2\n")(amap-contains-arg e "m1" "a1" "found")) #t 1 mystery3 101 #f)
           (Branch (lambda (e) (display "branch\n")(get-mapped e "found"))
                   (list 
                    (Instruction (lambda (e) (display "line: 3\n")(amap-get-arg e "m1" "a1" "val")) #t 1 mystery2 102 #f)
                    (Instruction  (lambda (e) (display "line: 4\n") (amap-remove-arg e "m1" "a1" "forget")) #t 1 mystery 103 #f))                   
                   (list))
           (Instruction (lambda (e) (display "line: 5\n") (update-mapped e "r0" (get-mapped e "val"))) #t 1 mystery5 104 #f))))

(define-struct Tuple (a b) #:transparent)



             
;; (add-optimistic concurrent-sketch)

(define-struct ERROR ())

(define (run-method-list instr-list env)
  (define (doloop condition instructions e depth)
    (let ([res (run-method-list instructions env)])
      (cond
        [(equal? depth 0) (display "Loop unrolled too far, breaking out!\n") (ERROR)]
        [else
         (if (condition res)
             (doloop condition instructions res (- depth 1))
             res)])))

  (if (empty? instr-list)
      env
      (match (first instr-list)
        [(Instruction f is-method id atomic inner-id rw?) (run-method-list (rest instr-list) (f env))]
        [(Loop condition loop-contents)
         (doloop condition loop-contents env 3)]
        [(Continue) env]
           
        [(Branch condition branch1 branch2) (let ([cond-result (and (condition env) (boolean? (condition env)))])

                                              (let ([branch-result (if cond-result (run-method-list branch1 env) (run-method-list branch2 env))])
                                                (run-method-list (rest instr-list) branch-result)))])))
        

(define (run-sequential-sketch-symbolic sketch test-env)
  (run-method-list (Method-instr-list sketch) test-env))
    
(define-symbolic a1 integer?)
(define-symbolic e1 e2 e3 integer?)


(define seq-test-env (list (list "m1" (AMap-entry e2 e1) (AMap-entry 1 7)) (cons "a1" a1)))
;; (run-sequential-sketch-symbolic (add-optimistic concurrent-sketch) seq-test-env)

;; (let ([test-env (list (list "m1" (AMap-entry e2 e1) (AMap-entry 1 7)) (cons "a1" a1))])
  ;; (synthesize #:forall (list)
  ;;             #:guarantee (assert (equivalent-envs? (run-sequential-sketch-symbolic (Tuple-b (add-optimistic concurrent-sketch)) test-env) (reference test-env))))


(define-symbolic* test-sym integer?)
(define concurrent-sketch-optim
  (Method "removeAttribute"
          (list
           (Instruction (lambda (e) (update-mapped e "optim" #t)) #t 1 0 99 #f)
           (Loop (lambda (e) (get-mapped e "optim"))
                 (list 
                  (Instruction (lambda (e) (update-mapped e "val" "Null")) #t 1 mystery4 100 #f)
                  (Instruction (lambda (e) (amap-contains-arg e "m1" "a1" "found")) #t 1 mystery3 101 #f)
                  (Branch (lambda (e) (get-mapped e "found"))
                          (list 
                           (if (= test-sym 0)
                               (Branch (choose* (lambda (e) (get-mapped e "found")) (lambda (e) (get-mapped e "z0")) )
                                       (list
                                        (Instruction (lambda (e) (amap-get-arg e "m1" "a1" "val")) #t 1 mystery2 102 #f)
                                        (Instruction (lambda (e) (update-mapped e "optim" #f)) #t 1 0 98 #f))
                                       (list))

                                (Branch (lambda (e) #t)
                                        (list (Instruction (lambda (e) (amap-get-arg e "m1" "a1" "val")) #t 1 mystery2 102 #f))
                                        (list))
                                )

                               ;; (Instruction (lambda (e) (amap-get-arg e "m1" "a1" "val")) #t 1 mystery2 102))
                               ;; (Instruction (lambda (e) (update-mapped e "optim" #f)) #t 1 0 98)
                               ;; (Instruction (lambda (e) e)))
                           

                           (Instruction (lambda (e) (amap-remove-arg e "m1" "a1" "forget")) #t 1 mystery 103 #f))
                   `())
                   
                    

                 (Instruction (lambda (e) (update-mapped e "r0" (get-mapped e "val"))) #t 1 mystery5 104 #f))))))



;; (define bindings (let ([test-env (list (list "m1" (AMap-entry 4 "A") (AMap-entry 1 7)) (cons "a1" 4))])
;;   (synthesize #:forall (list)
;;               #:guarantee (assert (equivalent-envs? (run-sequential-sketch-symbolic concurrent-sketch-optim test-env) (reference test-env))))))
;; bindings

;; (define (add-optimistic-helper instr-list)
;;   (define-symbolic* m integer?)
;;   (cond
;;     [(empty? instr-list) (Tuple `() `())]
;;     [(Branch? (first instr-list))
;;      (let ([condition (Branch-condition (first instr-list))] [branch1 (Branch-branch1 (first instr-list))]
;;            [branch2 (Branch-branch2 (first instr-list))]);; (Branch condition branch1 branch2)
;;        (let ([b1-res (add-optimistic-helper branch1)]
;;              [b2-res (add-optimistic-helper branch2)]
;;              [rec-result (add-optimistic-helper (rest instr-list))])
;;          (Tuple
;;           (append (Tuple-a b1-res) (Tuple-a b2-res) (Tuple-a rec-result))
;;           (append
;;            (list (Branch condition (Tuple-b b1-res) (Tuple-b b2-res)))
;;            (Tuple-b rec-result)))))]
;;     [else
;;      (let ([rec-result (add-optimistic-helper (rest instr-list))])
;;        (Tuple (append (list m) (Tuple-a rec-result))
;;               (append
;;                (if (equal? m 0)
;;                    (list (Branch (choose (lambda (e) (get-mapped (amap-contains-arg e "m1" "a1" "tmp") "tmp")))
;;                            (list
;;                             (Instruction (lambda (e) (update-mapped e "optim" #f)) #t 1 0 106)
;;                             (first instr-list))                           
;;                            `()))
;;                    (first instr-list))
;;                (Tuple-b rec-result))))]))


(define-synthax (choose-get keys depth)
  #:base (choose* (lambda (e) ;; (display e) (display "\n")(display (first keys)) (display "-")(display (get-mapped e (first keys))) (display "-")(if (and (get-mapped e (first keys)) (boolean? (get-mapped e (first keys)))) (display "true\n") (display "false\n"))


                          (and (boolean? (get-mapped e (first keys)) (get-mapped e (first keys))))))
  #:else (choose* (lambda (e) ;; (display e) (display "\n") (display (first keys)) (display "-")(display (get-mapped e (first keys))) (display "-")(if (and (get-mapped e (first keys)) (boolean? (get-mapped e (first keys)))) (display "true\n") (display "false\n"))

                          (and (boolean? (get-mapped e (first keys))) (get-mapped e (first keys)))
                 (choose-get (rest keys) (- depth 1)))))
  



(define-synthax (nnf x y depth)
 #:base (choose x (! x) y (! y))
 #:else (choose
         x (! x) y (! y)
         ((choose && ||) (nnf x y (- depth 1))
                         (nnf x y (- depth 1)))))


(define (add-optimistic-helper instr-list)
  (define-symbolic* m integer?)
  (cond 
    [(empty? instr-list) (Tuple `() `())]
    [(Branch? (first instr-list)) 
     (let ([condition (Branch-condition (first instr-list))]
           [branch1 (Branch-branch1 (first instr-list))]
           [branch2 (Branch-branch2 (first instr-list))])
       (let ([b1-res (add-optimistic-helper branch1)]
             [b2-res (add-optimistic-helper branch2)]
             [rec-result (add-optimistic-helper (rest instr-list))])
         (Tuple
          (append (Tuple-a b1-res) (Tuple-a b2-res) (Tuple-a rec-result))
          (append
           (list (Branch condition (Tuple-b b1-res) (Tuple-b b2-res)))
           (Tuple-b rec-result)))))]
    [else

     (let ([rec-result (add-optimistic-helper (rest instr-list))])
       (Tuple (append (list m) (Tuple-a rec-result))
              (append
               (list (Branch (lambda (e) (= 1 m))
                   (list
                    (Branch (choose* (lambda (e) #f) (lambda (e) (get-mapped e "found")))
                            (list
                             (first instr-list)
                             (Instruction (lambda (e) (display "updated optim here: ") (display e) (display "\n")(update-mapped e "optim" #f)) #t 1 0 98 #f))
                            (list
                             (Continue))))
                   (list
                    (Branch (lambda (e) #t)
                            (list (first instr-list))
                            (list)))))
                                  
               (Tuple-b rec-result))))]))

    

(define (add-optimistic sketch)
  (let ([instr-list (Method-instr-list sketch)] [name (Method-id sketch)])
    (Method
     name
     (append
      (list (Instruction (lambda (e) (update-mapped e "optim" #t)) #t 1 0 105 #f))
      (list (Loop (lambda (e) (get-mapped e "optim"))
                  (let ([opt-helper-res (add-optimistic-helper instr-list)])
                    (Tuple-b opt-helper-res))))))))


(define (is-rw-call? instr)
  #t)

(define (Make-announcement instr)
  instr)


(define (add-announcements sketch)
  (define-symbolic* do-announcement? integer?)
  (define (add-announcements-helper instr-list)
    (cond
      [(empty? instr-list) `()]
      [(Branch? (first instr-list))
       (let ([condition (Branch-condition (first instr-list))]
             [branch1 (Branch-branch1 (first instr-list))]
             [branch2 (Branch-branch2 (first instr-list))])
         (let ([b1-res (add-announcements-helper branch1)]
               [b2-res (add-announcements-helper branch2)]
               [rec-result (add-announcements-helper (rest instr-list))])
           (Branch condition b1-res b2-res)))]
      [(is-rw-call? (first instr-list))
       (cond
         [(equal? do-announcement? 1)
          (let ([rec-result (add-announcements-helper (rest instr-list))])
            (append
             (list (Make-announcement (first instr-list))
                   (first instr-list))
             rec-result))]
         [else
          (append (list (first instr-list)) (add-announcements-helper (rest instr-list)))])]))

  
  (let ([instr-list (Method-instr-list sketch)] [name (Method-id sketch)])
    (Method
     name
     (add-announcements-helper instr-list))))

;; (define bindings (let ([test-env (list (list "m1" (AMap-entry 4 "A") (AMap-entry 1 7)) (cons "a1" 4))])
;;   (synthesize #:forall (list)
;;               #:guarantee (assert (equivalent-envs? (run-sequential-sketch-symbolic (add-optimistic concurrent-sketch) test-env) (reference test-env))))))
;; bindings

;; (add-ids-to-instruction-lists 
;; (get-interleavings-as-lists
  ;; (generate-interleavings
 ;; (replace-calls concurrent-client (add-optimistic concurrent-sketch)) (History-op-list concurrent-history))
;; ;; bindings
;; ((Branch-condition (first (Branch-branch1 (third (Loop-instr-list (second (Method-instr-list (add-optimistic concurrent-sketch))))))))
;;  (list (cons "z0" true) (cons "r0" false)))

;; (define branchMethod (Method
;;  "something"
;;   (list (Branch (lambda (e) #t)
;;                 (list (Instruction (lambda (e) (update-mapped e "found" #t)) #t 1 0 0))
;;                 (list))
;;         (Branch (lambda (e) #t)
;;                 (list (Instruction (lambda (e) (update-mapped e "cat" "dog")) #t 1 0 0))
;;                 (list)))))


                

(define test-env (list (list "m1" (AMap-entry 4 "A") (AMap-entry 1 7)) (cons "a1" 4)))
;; (length  (generate-simulation concurrent-client (add-optimistic concurrent-sketch) concurrent-history))
;; (run-sequential-sketch-symbolic (add-optimistic concurrent-sketch) test-env)
(synthesize #:forall (list)
            #:guarantee (assert
                        (equivalent-envs? (run-sequential-sketch-symbolic (add-optimistic concurrent-sketch) test-env)
                                         (reference test-env))))
;; (run-sequential-sketch-symbolic (add-optimistic concurrent-sketch) test-env)
;; ((Instruction-i (first (Branch-branch1 (second (Loop-instr-list (second (Method-instr-list (add-optimistic concurrent-sketch)))))))) test-env)

;; (let ([test-env (list (list "m1" (AMap-entry 4 "A") (AMap-entry 1 7)) (cons "a1" 4))])
;;   (run-sequential-sketch-symbolic (add-optimistic concurrent-sketch) test-env))


;; bindings
;; ((choose-get (list  "z0" "r0") 1) (list (cons "z0" 50) (cons "r0" 40)))


;; (let ([test-env (list (list "m1" (AMap-entry 4 "A") (AMap-entry 1 7)) (cons "a1" 4))])
;;   (run-sequential-sketch-symbolic concurrent-sketch-optim test-env))



;; ((Branch-condition (first (Branch-branch1 (fourth (Loop-instr-list (second (Method-instr-list (add-optimistic concurrent-sketch))))))))
 ;; seq-test-env)


(define (cex-guided-repair-cycle client sketch history)
        
        (define (loop client sketch history)
          (let ([sim (generate-simulation client sketch history)])

            ;; (display (first (interested-vals (run-lists sim (History-start-env history))))) (display "\n\n\n")
            (let ([env-list (interested-vals (run-lists sim (History-start-env history)))])
              (let ([history-correct-envs (valid-environments history)])

                ;; (display (get-mapped (second env-list) "possible")) (display "\n")
                (let ([binding (synthesize #:forall (list)
                                           #:guarantee (verify-sketch-with-history sim history-correct-envs env-list client sketch history))])
                  binding)))))
                ;;   (cond
                ;;     [(unsat? binding)

                ;;      (display "unsat\n")]
                     ;; (let ([new-cex (find-counter-example sim env-list history-correct-envs)])
                     ;;   (display new-cex) (display "\n"))]
                       ;; (let ([new-sketch (counter-example-atomic-fix new-cex sketch)])))]
                         ;; (display (atomic? (second (Branch-branch1 (third (Method-instr-list new-sketch)))))) (display "\n")
                         ;; (display (atomic? (first (Branch-branch1 (third (Method-instr-list new-sketch)))))) (display "\n")
                         ;; (loop cex client new-sketch history)))]
                    ;; [else binding]))))))
        (loop client sketch history))
  
;; (cex-guided-repair-cycle concurrent-client concurrent-sketch concurrent-history)

;; (amap-remove-arg e "m1" "a1" "z0")

(define (all-map-methods shared-obj)
  (define-symbolic* to-remove integer?)
  (define-symbolic* to-add-key integer?)
  (define-symbolic* to-add-val integer?)
  (let ([new-ret-var "r1"])
    (list
     (lambda (env) (amap-remove shared-obj to-remove new-ret-var))
     (lambda (env) (amap-put shared-obj to-add-key to-add-val new-ret-var)))))

(define (get-operations obj)
  (all-map-methods obj))


(define (get-all-enabled-operations all-shared-vars)
  (if (empty? all-shared-vars)
      (list)
      (append (get-operations (first all-shared-vars)) (get-all-enabled-operations (rest all-shared-vars)))))

(define (all-clients-up-to enabled-ops depth)
  (cond
    [(empty? enabled-ops) (list (list))]
    [(equal? depth 0) (list (list))]
    [else
      (append (map (lambda (x) x) (all-clients-up-to (rest enabled-ops) depth))
              (map (lambda (x) (append (list (first enabled-ops)) x)) (all-clients-up-to enabled-ops (- depth 1))))]))

    

;; (define concurrent-client
;;   (Client-pre
;;    (list
;;     (Thread-list
;;      (list
;;       (Client-pre
;;        (list
;;         (Sketch-placeholder "removeAttribute")))
;;       (Client-pre
;;        (list
;;         (Instruction (lambda (e) (amap-remove-arg e "m1" "a1" "z0")) #f 0 0 0))))))))
       

(define (build-client sketch-name all-shared-vars depth)
  (define (transform-to-client proc-list)
    (Client-pre
     (list
      (Thread-list
       (list
        (Client-pre
         (list (Sketch-placeholder sketch-name)))
        (Client-pre
          (map (lambda (proc) (Instruction proc #f depth 0 0 #f)) proc-list)))))))
    
  (let ([proc-lists (all-clients-up-to (get-all-enabled-operations all-shared-vars) depth)])
    (map transform-to-client (filter (lambda (x) (not (empty? x))) proc-lists))))



;; (for-each (lambda (x) (cex-guided-repair-cycle x concurrent-sketch concurrent-history)) (build-client "removeAtribute" (list "m1") 1))





 ;; (all-clients-up-to (get-all-enabled-operations (list "m1" "m2")) 1)


;; (length (generate-client-instr-list (list "m1") 3)) ;; TODO: This is wrong...

;; (define-struct Tuple (a b))
;; (define (generate-clients all-shared-vars up-to)
;;   (define (append-each l1 l2)
;;     (if (empty? l1)
;;         (list (list))
;;         (append
;;          (append (list (first l1)) l2)
;;          (append-each (rest l1) l2)))
;;     (append-each (get-all-possible-operations (first all-shared-vars)) (generate-clients 



     
  
  
;; (define binding
;;     (synthesize #:forall (list)
;;                 #:guarantee (assert (= (length conc-sim) 10))))
;; binding




;; (valid-environments concurrent-history)


   ;; (get-interleavings-as-lists (generate-interleavings (replace-calls concurrent-client concurrent-sketch) (History-op-list concurrent-history)))
;; (define conc-sim (generate-simulation concurrent-client concurrent-sketch concurrent-history))
;; (for-each
;;  (lambda (l) (display "r0: ") (display (get-mapped l "r0")) (display "--- z0: ") (display (get-mapped l "z0")) (display "---- id: ") (display (get-mapped l "id")) (display "\n"))
;;  (run-lists conc-sim conc-env))



;; Example sketch!
;; (define choice1 (??))
;; (define choice2 (??))
;; (define example-sketch
;;   (Method "newMethod"
;;           (list
;;            (Instruction (lambda (e)
;;                           (cond
;;                             [(= x 0) (list-add e "l1" "z0")]
;;                             [(= x 1) (list-add e "l1" "z1")]
;;                             [(= x 2) (list-remove e "l1" 0 "z6")])) #t 0)

;;            (Instruction (lambda (e)
;;                           (cond
;;                             [(= y 0) (list-add e "l1" "z0")]
;;                             [(= y 1) (list-add e "l1" "z1")]
;;                             [(= y 2) (list-remove e "l1" 0 "z7")])) #t 0))))

;; (define example-client
;;   (Client-pre
;;    (list
;;     (Thread-list
;;      (list
;;       (Client-pre
;;        (list
;;         (Sketch-placeholder "newMethod")))
;;       (Client-pre
;;        (list
;;         (Instruction (lambda (e) (list-add e "l1" "z5")) #f 1)
;;         (Instruction (lambda (e) (list-remove e "l1" 4 "z6")) #f 2))))))))


;; (define (reference e)
;;   (let ([newE (list-add e "l1" "z0")])
;;     (list-remove newE "l1" 0 "z7")))

;; (define example-history
;;   (History env
;;    (list
;;    (Operation
;;     (lambda (e) (list-add e "l1" "z5")) (Interval 0 3) 1)
;;    (Operation (lambda (e) (newMethod e)) (Interval 2 5) 0)
;;    (Operation (lambda (e) (list-remove e "l1" 4 "z6")) (Interval 6 11) 2))))




;;;;;;;;;;;;;;;;;;;;;;;;; Client generation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (run-lists ex-sim env)


;; binding
    


;; (valid-environments example-history)
    

;; (implements-history (Instruction-sequence-seq (list-ref sim 3)) (History-op-list example-history))
;; (all-that-implement sim example-history)
;; (run-lists (generate-simulation example-client example-sketch) (list (list "l1" 1 2 3) (list "z0" 5) (list "z1" 6)))



;;;;;;;;;;;;;;;;;;;; Lets try some synthesis! ;;;;;;;;;;




;; (define trace-example (first (get-interleavings-as-lists (generate-interleavings ret))))

;; (implements-history trace-example (History-op-list test-hist))


