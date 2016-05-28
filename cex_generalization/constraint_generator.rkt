#lang rosette/safe
(require rosette/lib/synthax)
(require racket/dict)
(require (only-in racket make-hash))

(require racket/match)
(require racket/string)
(require rosette/lib/angelic)
(require "../program_representation/concurrent-object-lib.rkt")
(require "../program_representation/simulator-structures.rkt")
(require racket/trace)

(define (append-item item)
  (lambda (l) (append (list item) l)))









;; Example client
(define client-pre-example
  (Client-pre
   (list
    (Thread-list
     (list
      (Client-pre (list (Instruction (lambda (env) (list-add env "l1" 1)) #f 0 0 0 #f (Empty)))) ;; TODO option not to return
      (Client-pre (list (Sketch-placeholder "newMethod"))))))))

       
;; Example method
(define method-example
  (Method "newMethod"
   (list (Instruction (lambda (env) (list-add env "l1" 12 )) #t 1 0 0 #f (Empty))
    (Instruction (lambda (env) (list-remove env "l1" 0 "z1")) #t 1 0 0 #f (Empty)))))


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
    [(Assume? instr1) #f]
    [(Assume? instr2) #f]
    [(and (Instruction? instr1) (Instruction? instr2))

     
     (let ([id1 (Instruction-id instr1)] [id2 (Instruction-id instr2)])

       (let ([op1 (find-operation-with-id h-list id1)] [op2 (find-operation-with-id h-list id2)])
         (let ([interval1 (Operation-interval op1)] [interval2 (Operation-interval op2)])
           ;; (display (Instruction-inner-id instr1)) (display "-") (display (Instruction-inner-id instr2)) (display "-")
           ;; (display (> (Interval-start interval2) (Interval-end interval1)))
           ;; (display "\n")
           (> (Interval-start interval2) (Interval-end interval1)))))]
    ;; (let ([interval1 (Operation-interval op1)] [interval2 (Operation-interval op2)])
    ;;   (> (Interval-start op2) (Interval-end op1))))
    [else
     (display "SHOULD NOT HAPPEN\n")]))
    


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
;; (define tracker (void))
;; (set! tracker 0)
(define (unpack-branches l)
  ;; (set! tracker (+ tracker 1))
  ;; (display "Unpack called: ") (display tracker) (display " times - length: ") (display (length l)) (display "\n")
  ;; (display "unpacking\n")
  
  (define (append-list l)
    (lambda (l2)
      (append l l2)))
  (cond
    [(empty? l) (list (list))]
    [(Single-branch? (first l))
     (append
      (list (list (Assume-simulation (Single-branch-condition (first l)))))
      (unpack-branches (append
                        (list (Assume-simulation (lambda (e) (not ((Single-branch-condition (first l)) e)))))
                        (rest l))))]
    [(Loop? (first l)) ;; (display "unpack: ") (display (reduce append (map unpack-branches (all-unrolls (first l) 0)))) (display "\n")

     (reduce append (map unpack-branches (all-unrolls (first l) 2)))]
    [(Assume? (first l)) (map (append-item (first l)) (unpack-branches (rest l)))]
    [(Branch? (first l))
     (append
      (unpack-branches (append (list (Assume-simulation (Branch-condition (first l)))) (Branch-branch1 (first l)) (rest l)))
      (unpack-branches (append (list (Assume-simulation (lambda (e) (not ((Branch-condition (first l)) e))))) (Branch-branch2 (first l)) (rest l))))]
    [(Meta-branch? (first l))
     ;; (display "Meta branching: ") (display "first - ")(display ((Meta-branch-condition (first l)) `())) (display "... second - ") (display ((lambda (e) (not ((Meta-branch-condition (first l)) e))) `())) (display "\n")
     (append
      (unpack-branches (append (list (Assume-meta (Meta-branch-condition (first l)))) (Meta-branch-branch1 (first l)) (rest l)))
      (unpack-branches (append (list (Assume-meta (lambda (e) (not ((Meta-branch-condition (first l)) e))))) (Meta-branch-branch2 (first l)) (rest l))))]

      ;; (all-list-combos (filter (lambda (l) (not (empty? l))) (unpack-branches (Branch-branch1 (first l)))) (unpack-branches (rest l)))
      ;; (all-list-combos (filter (lambda (l) (not (empty? l))) (unpack-branches (Branch-branch2 (first l)))) (unpack-branches (rest l))))]
      ;; (map (append-list (Branch-branch2 (first l))) (unpack-branches (rest l))))]
    [else
     (map (append-item (first l)) (unpack-branches (rest l)))]))
    
  
(define (atomic? instr)
  (not (= (Instruction-atomic instr) 0)))



(define (is-possible-execution? l)
  
  (cond
    [(> 2 (length l))  #t]
    [(and (Assume? (first l)) (Instruction? (second l)))              ;; (is-possible-execution? (rest l))]
     ;; TODO - THIS IS TEMPORARY - we assume any assume comes from method 1
     (if (not (equal? (Instruction-id (second l)) 1))
         ;; (begin (display "Context switched after assume\n") #f)
         #f
         (is-possible-execution? (rest l)))]
    [(and (Instruction? (first l)) (Assume? (second l)))
     (if (not (equal? (Instruction-id (first l)) 1))
         ;; (begin (display "Context switched before assume\n") #f)
         #f
         (is-possible-execution? (rest l)))]

    [(and (Assume? (first l)) (Assume? (second l)))

     (is-possible-execution? (rest l))]

    [(and (Instruction? (first l)) (Instruction? (second l)))
     ;; (display "possible: ") (display (not (and (atomic? (first l)) (not (equal? (Instruction-id (second l)) (Instruction-id (first l))))))) (display "-") (display (Instruction-id (second l))) (display "-") (display (Instruction-id (first l))) (display "-atom:") (display (atomic? (first l))) (display "\n")
     ;; (display "Comparing: ") (display (Instruction-inner-id (first l))) (display "-") (display (Instruction-inner-id (second l))) (display "-") (display (not (and (atomic? (first l)) (not (equal? (Instruction-id (second l)) (Instruction-id (first l))))))) (display "\n")
     ;; (display "checking two instructions... ") (display      
     ;;       (not (and (atomic? (first l)) (not (equal? (Instruction-id (second l)) (Instruction-id (first l)))))))
     ;; (display "\n")

     (and
           (not (and (atomic? (first l)) (not (equal? (Instruction-id (second l)) (Instruction-id (first l))))))
           (is-possible-execution? (rest l)))]
    [else
     (display "IS POSSIBLE EXECUTION IS WRONG!!! ") (display (first l)) (display (second l)) (display "\n")]))




;; For a single loop which has some number of Single-branch nodes which
;; restart the loop if the condition evaluates to true
;; This generates all possible results of this sequence happening once
(define (possible-single-loop-executions l)
  (cond
    [(empty? l) (list (list))]
    [(Single-branch? (first l))
     (append
      (list (list (Assume-simulation (Single-branch-condition (first l)))))
      (append (list (Assume-simulation (lambda (e) (not ((Single-branch-condition (first l)) e))))) (possible-single-loop-executions (rest l))))]
    [else
     (map (append-item (first l)) (possible-single-loop-executions (rest l)))]))
   



(define (all-appends l-set1 l-set2)
  (define (append-list l)
    (lambda (l2)
      (append l l2)))
  (cond
    [(empty? l-set1) (list (list))]
    [(empty? l-set2) (list (list))]
    [else
     (append
      (map (append-list (first l-set1)) l-set2)
      (all-appends (rest l-set1) l-set2))]))
      

(define (unroll-loop loop depth)
  (define (not-empty l)
    (not (empty? l)))
  (cond
    [(equal? depth 0) (list (list (Assume-simulation (lambda (e)  ;; (display (not ((Loop-condition loop) e))) (display "\n")
                                                  (not ((Loop-condition loop) e))))))]
    [else
     (let ([possible-executions (possible-single-loop-executions (Loop-instr-list loop))])
       (map (append-item (Assume-simulation (Loop-condition loop))) (filter not-empty (all-appends possible-executions (unroll-loop loop (- depth 1))))))]))
     
(define (range n)
  (if (= n 0)
      (list 0)
      (append (list n) (range (- n 1)))))


(define (all-unrolls loop depth)
  (cond
    [(equal? depth -1) (list)]
    [else
     (display "unroll: ")(display (unroll-loop loop depth)) (display "\n")
     (append
           (unroll-loop loop depth)
           (all-unrolls loop (- depth 1)))]))
     
   
;; (define example-loop
;;   (Loop
;;    (lambda (e) #f)
;;    (list
;;     1
;;     2
;;     3
;;     (Single-branch (lambda (e) #f)
;;                    (list "3A"))
;;     4
;;     5
;;     (Single-branch (lambda (e) #t)
;;                    (list "3A")))))
;; ;; (possible-single-loop-executions example-loop)
;; (all-unrolls example-loop 2)
      


                  
;; (define (unroll-loop loop depth)

;;   (cond
;;     [(equal? depth 0) (list (Assume (lambda (e) (not ((Loop-condition loop) e)))))]
;;     [else
;;      (append (list (Assume (Loop-condition loop)))  (Loop-instr-list loop) (unroll-loop loop (- depth 1)))]))




;; (define (all-unrolls loop depth)
;;   (define (unroll-loop-with-arg n)
;;     (unroll-loop loop n))
;;   (map unroll-loop-with-arg (range depth)))
(define (reduce func list)
  (assert (not (null? list)))
  (if (null? (cdr list))
      (car list)
      (func (car list) (reduce func (cdr list)))))



;; (define (cex-hole-aware-shuffle l1 l2 hole-list)
;;   (define (append-list l)
;;     (lambda (l2)
;;       (append l l2)))
;;   ;; (display "l1: ") (map (lambda (i) (display (cond [(Instruction? i) (Instruction-inner-id i)] [(Assume? i)  "ASSUME"] [else "BRANCH"])) (display "-")) l1) (display "\n")
;;   ;; (display "l2: ") (map (lambda (i) (display (cond [(Instruction? i) (Instruction-inner-id i)] [(Assume? i)  "ASSUME"] [else "BRANCH"])) (display "-")) l2) (display "\n")
;;   ;; (display "h-list: ") (display h-list) (display "\n")
;;   (cond
;;     [(empty? l1) ;; (display (unpack-branches l2)) 
;;      (unpack-branches l2)] ;; TODO - the problem is (rest l1) doesn't unpack all the branches!
;;     [(empty? l2) ;; (display (unpack-branches l1)) (display "unpacking l1" ) (display l1)(display "\n") 
;;      (unpack-branches l1)]
;;     [(Loop? (first l1))
;;      ;; (display "loop: ")
;;      ;; (display (reduce append (map (lambda (l) (cex-hole-aware-shuffle (append l (rest l1)) l2 h-list)) (all-unrolls (first l1) 1)))) (display "\n")
;;      (reduce append (map (lambda (l) (cex-hole-aware-shuffle (append l (rest l1)) l2 h-list)) (all-unrolls (first l1) 2)))]
;;       ;; (cex-hole-aware-shuffle (append (unroll-loop (first l1) 0) (rest l1)) l2 h-list))]

;;     ;;   (cex-hole-aware-shuffle (append (unroll-loop  (first l1) 3) (rest l1)) l2 h-list)]
;;     [(Single-branch? (first l1))
;;      ;; (display "single-branch: ")
;;      ;; (display (Single-branch-branch (first l1))) (display "\n")
;;      (append
;;       (list (list (Assume-simulation (Single-branch-condition (first l1)))))
;;       (cex-hole-aware-shuffle (append (list (Assume-simulation (lambda (e) (not ((Single-branch-condition (first l1)) e))))) (rest l1)) l2 h-list))]
;;       ;; (cex-hole-aware-shuffle (append (Single-branch-branch (first l1)) (rest l1)) l2 h-list)
;;       ;; (cex-hole-aware-shuffle (rest l1) l2 h-list))]
;;     [(Branch? (first l1)) ;; TODO - There are no assume statements.
;;      (append
;;        (cex-hole-aware-shuffle (append (list (Assume-simulation (Branch-condition (first l1)))) (Branch-branch1 (first l1)) (rest l1)) l2 h-list)
;;        (cex-hole-aware-shuffle (append (list (Assume-simulation (lambda (e) (not ((Branch-condition (first l1)) e))))) (Branch-branch2 (first l1)) (rest l1)) l2 h-list))]
;;     [(Branch? (first l2))
;;      (append
;;       (cex-hole-aware-shuffle l1 (append (Branch-branch1 (first l2)) (rest l2)) h-list)
;;       (cex-hole-aware-shuffle l1 (append (Branch-branch2 (first l2)) (rest l2)) h-list))]


;;     [(Meta-branch? (first l1))
;;      ;; (display "SHUFFLING META BRANCH:\n")
;;      ;; (display "BRANCH1: ") (      (cex-hole-aware-shuffle (append (list (Assume-meta (Meta-branch-condition (first l1)))) (Meta-branch-branch1 (first l1)) (rest l1)) l2 h-list)) (display "\n")
;;      ;; (display "BRANCH2: ") (display      (cex-hole-aware-shuffle (append (list (Assume-meta (lambda (e) (not ((Meta-branch-condition (first l1)) e))))) (Meta-branch-branch2 (first l1)) (rest l1)) l2 h-list)) (display "\n") 
;;           ;; (display "Meta branching: ") (display "first - ")(display ((Meta-branch-condition (first l1)) `())) (display "... second - ") (display ((lambda (e) (not ((Meta-branch-condition (first l1)) e))) `())) (display "\n")
;;      (append (cex-hole-aware-shuffle (append (list (Assume-meta (Meta-branch-condition (first l1)))) (Meta-branch-branch1 (first l1)) (rest l1)) l2 h-list)
;;      (cex-hole-aware-shuffle (append (list (Assume-meta (lambda (e) (not ((Meta-branch-condition (first l1)) e))))) (Meta-branch-branch2 (first l1)) (rest l1)) l2 h-list))]
;;     ;; [(atomic? (first l1))
;;     ;;  (map (append-item (first l1)) (cex-hole-aware-shuffle (rest l1) l2 h-list))]
;;     ;; [(atomic? (first l2))
;;     ;;  (map (append-item (first l2)) (cex-hole-aware-shuffle l1 (rest l2) h-list))]
;;     [(happens-before h-list (first l1) (first l2)) ;; (display "happens before (first l1) (first l2)\n")
;;      (map (append-item (first l1)) (cex-hole-aware-shuffle (rest l1) l2 h-list))]
;;     [(happens-before h-list (first l2) (first l1)) ;; (display "happens before (first l2) (first l1)\n")
;;      (map (append-item (first l2)) (cex-hole-aware-shuffle l1 (rest l2) h-list))]
;;     [else ;; (display "no happens before-")
;;           (let ([all-shuffles2 (map (append-item (first l2)) (cex-hole-aware-shuffle l1 (rest l2) h-list))]
;;                 [all-shuffles1 (map (append-item (first l1)) (cex-hole-aware-shuffle (rest l1) l2 h-list))])
;;             ;; (display "all-shuffles: " ) (display (append all-shuffles1 all-shuffles2)) (display "\n")
;;             (append all-shuffles1 all-shuffles2))]))






(define (history-aware-shuffles l1 l2 h-list)
  (define (append-list l)
    (lambda (l2)
      (append l l2)))
  ;; (display "l1: ") (map (lambda (i) (display (cond [(Instruction? i) (Instruction-inner-id i)] [(Assume? i)  "ASSUME"] [else "BRANCH"])) (display "-")) l1) (display "\n")
  ;; (display "l2: ") (map (lambda (i) (display (cond [(Instruction? i) (Instruction-inner-id i)] [(Assume? i)  "ASSUME"] [else "BRANCH"])) (display "-")) l2) (display "\n")
  ;; (display "h-list: ") (display h-list) (display "\n")
  (cond
    [(empty? l1) ;; (display (unpack-branches l2)) 
     (unpack-branches l2)] ;; TODO - the problem is (rest l1) doesn't unpack all the branches!
    [(empty? l2) ;; (display (unpack-branches l1)) (display "unpacking l1" ) (display l1)(display "\n") 
     (unpack-branches l1)]
    [(Loop? (first l1))
     ;; (display "loop: ")
     ;; (display (reduce append (map (lambda (l) (history-aware-shuffles (append l (rest l1)) l2 h-list)) (all-unrolls (first l1) 1)))) (display "\n")
     (reduce append (map (lambda (l) (history-aware-shuffles (append l (rest l1)) l2 h-list)) (all-unrolls (first l1) 2)))]
      ;; (history-aware-shuffles (append (unroll-loop (first l1) 0) (rest l1)) l2 h-list))]

    ;;   (history-aware-shuffles (append (unroll-loop  (first l1) 3) (rest l1)) l2 h-list)]
    [(Single-branch? (first l1))
     ;; (display "single-branch: ")
     ;; (display (Single-branch-branch (first l1))) (display "\n")
     (append
      (list (list (Assume-simulation (Single-branch-condition (first l1)))))
      (history-aware-shuffles (append (list (Assume-simulation (lambda (e) (not ((Single-branch-condition (first l1)) e))))) (rest l1)) l2 h-list))]
      ;; (history-aware-shuffles (append (Single-branch-branch (first l1)) (rest l1)) l2 h-list)
      ;; (history-aware-shuffles (rest l1) l2 h-list))]
    [(Branch? (first l1)) ;; TODO - There are no assume statements.
     (append
       (history-aware-shuffles (append (list (Assume-simulation (Branch-condition (first l1)))) (Branch-branch1 (first l1)) (rest l1)) l2 h-list)
       (history-aware-shuffles (append (list (Assume-simulation (lambda (e) (not ((Branch-condition (first l1)) e))))) (Branch-branch2 (first l1)) (rest l1)) l2 h-list))]
    [(Branch? (first l2))
     (append
      (history-aware-shuffles l1 (append (Branch-branch1 (first l2)) (rest l2)) h-list)
      (history-aware-shuffles l1 (append (Branch-branch2 (first l2)) (rest l2)) h-list))]


    [(Meta-branch? (first l1))
     ;; (display "SHUFFLING META BRANCH:\n")
     ;; (display "BRANCH1: ") (      (history-aware-shuffles (append (list (Assume-meta (Meta-branch-condition (first l1)))) (Meta-branch-branch1 (first l1)) (rest l1)) l2 h-list)) (display "\n")
     ;; (display "BRANCH2: ") (display      (history-aware-shuffles (append (list (Assume-meta (lambda (e) (not ((Meta-branch-condition (first l1)) e))))) (Meta-branch-branch2 (first l1)) (rest l1)) l2 h-list)) (display "\n") 
          ;; (display "Meta branching: ") (display "first - ")(display ((Meta-branch-condition (first l1)) `())) (display "... second - ") (display ((lambda (e) (not ((Meta-branch-condition (first l1)) e))) `())) (display "\n")
     (append (history-aware-shuffles (append (list (Assume-meta (Meta-branch-condition (first l1)))) (Meta-branch-branch1 (first l1)) (rest l1)) l2 h-list)
     (history-aware-shuffles (append (list (Assume-meta (lambda (e) (not ((Meta-branch-condition (first l1)) e))))) (Meta-branch-branch2 (first l1)) (rest l1)) l2 h-list))]
    ;; [(atomic? (first l1))
    ;;  (map (append-item (first l1)) (history-aware-shuffles (rest l1) l2 h-list))]
    ;; [(atomic? (first l2))
    ;;  (map (append-item (first l2)) (history-aware-shuffles l1 (rest l2) h-list))]
    [(happens-before h-list (first l1) (first l2)) ;; (display "happens before (first l1) (first l2)\n")
     (map (append-item (first l1)) (history-aware-shuffles (rest l1) l2 h-list))]
    [(happens-before h-list (first l2) (first l1)) ;; (display "happens before (first l2) (first l1)\n")
     (map (append-item (first l2)) (history-aware-shuffles l1 (rest l2) h-list))]
    [else ;; (display "no happens before-")
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
    [(Assume? (first seq)) (append (list (if (Assume-meta? (first seq)) "META" "ASSUME")) (calculate-sequence-id (rest seq)))]
    [(or (Continue? (first seq)) (Branch? (first seq)) (Single-branch? (first seq))) (calculate-sequence-id (rest seq))]
    [else (append (list (Instruction-inner-id (first seq))) (calculate-sequence-id (rest seq)))]))


(define (add-ids-to-instruction-lists inst-seq-list)
  ;; (map (lambda (i)
  ;;  (display (is-possible-execution? i)) (display "\n"))
  ;;      inst-seq-list)
  
  (map (lambda (i)
         (Instruction-sequence (is-possible-execution? (filter (lambda (elem) (not (Assume-meta? elem))) i)) (calculate-sequence-id (filter (lambda (elem) (not (Assume-meta? elem))) i)) i))
       inst-seq-list))

  ;; (map (lambda (i)
  ;;        (Instruction-sequence (is-possible-execution? i) (calculate-sequence-id i) i))
  ;;      inst-seq-list))

  
  ;; (define (add-id inst-list i)
  ;;   (if (empty? inst-list)
  ;;       `()
  ;;       (append (list (Instruction-sequence
  ;;                      (is-possible-execution? (first inst-list))
  ;;                      (calculate-sequence-id (first inst-list))
  ;;                      (first inst-list)))
  ;;               (add-id (rest inst-list) (+ 1 i)))))

  
  ;; (add-id inst-seq-list 0))
    


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
    ;; (display e) (display "\n")
    (cond
      [(empty? l) (add-entry (add-entry e "possible" possible?) "id" id)]
      [(Assume? (first l)) ;; (display "ASSUMING: ") (display "(meta: ") (display (Assume-meta? (first l))) (display ")") (display ((Assume-condition (first l)) e)) (display "\n")
       (run-list (rest l) e id (and possible? ((Assume-condition (first l)) e)))]
       
       ;; (if ((Assume-condition (first l)) e)
       ;;                         (run-list (rest l) e id possible?)
       ;;                         (run-list (rest l) e id (and possible? #f)))]
                               ;; (list (cons "possible" #f)))]


                               ;; (add-entry (add-entry e "possible" #f) "id" id))]

                            
      [else ;; (display "running instruction:") (display (Instruction-inner-id (first l))) (display "--- ") (display e) (display "\n")
       (run-list (rest l) ((Instruction-i (first l)) e) id possible?)]))
  
  
  ;; (define (run-list l e id possible?)
  ;;   (case
  ;;    [(empty? l) (add-entry (add-entry e "possible"  possible?) "id" id)]


  ;;    [else (run-list (rest l) ((Instruction-i (first l)) e) id possible?)]))  ;; ((Instruction-i  (first l)) e)) (rest l) )))
    ;; (cond
    ;;   [(empty? l) (add-entry e "id" id)]
    ;;   [((run-list ((Instruction-i (first l)) e)) (rest l))]))

  (map (lambda (l)

         ;; (if (Instruction-sequence-possible? l)
         ;;     (begin (display (run-list (Instruction-sequence-seq l) env (Instruction-sequence-id l) (Instruction-sequence-possible? l))) (display "\n"))
         ;;     (void))
         ;; (display "NEW LIST: ") (display (Instruction-sequence-id l)) (display "\n")
               (run-list (Instruction-sequence-seq l) env (Instruction-sequence-id l) (Instruction-sequence-possible? l)))
       list-set))
  ;; (cond
  ;;   [(empty? list-set) (list)]
  ;;   [else
  ;;    (let
  ;;        ([instr-seq (Instruction-sequence-seq (first list-set))]
  ;;         [instr-seq-id (Instruction-sequence-id (first list-set))]
  ;;         [instr-seq-possible (Instruction-sequence-possible? (first list-set))])
  ;;      ;; ;; (display instr-seq-possible) (display "-")
  ;;      ;; (display "Running sequence: ") (display instr-seq-id) (display "\n")
  ;;      ;; (if (equal? instr-seq-id (list 0 105))
  ;;      ;;     (let ([new-env ((Instruction-i (first instr-seq)) env)])
  ;;      ;;       (let ([new-env2 ((Instruction-i (second instr-seq)) new-env)])
  ;;      ;;         (begin (display "FOUND 0 105: ")(display new-env2) (display "-")
  ;;      ;;                (display ((Assume-condition (third instr-seq)) (list (cons "optim" #t))))
  ;;      ;;                (display "\n")
  ;;      ;;                (display "Result: ") (display (run-list instr-seq env instr-seq-id instr-seq-possible)) (display "\n")
  ;;      ;;                )))
  ;;      ;;     (void))
  ;;      ;; (map (lambda (i) (display (Instruction-inner-id i)) (display "--"))
  ;;      ;;      instr-seq)
  ;;      ;;       (display "\n")


  ;;      (append
  ;;       (list (run-list instr-seq env instr-seq-id instr-seq-possible))
  ;;       (run-lists (rest list-set) env)))]))
       



;; (define ret (replace-calls client-pre-example method-example))


;; (run-lists (add-ids-to-instruction-lists (get-interleavings-as-lists (generate-interleavings ret))) (list (list "l1" 1 2 3)))
;; (add-ids-to-instruction-lists (get-interleavings-as-lists (generate-interleavings ret)))




;; Generates a set of Instruction sequences that can be run on an environement from a Client-pre and a method sketch
(define (generate-simulation client method-sketch history)
  ;; (display (Instruction-sequence-id (third (add-ids-to-instruction-lists (get-interleavings-as-lists (generate-interleavings (replace-calls client method-sketch) (History-op-list history)))))))
  ;; (display "\n") 
  ;; (display (is-possible-execution? (Instruction-sequence-seq (third (add-ids-to-instruction-lists (get-interleavings-as-lists (generate-interleavings (replace-calls client method-sketch) (History-op-list history))))))))
  ;; (display "\n____________\n")

  (let ([part1 (replace-calls client method-sketch)])
    (display "part1 done\n")
    (let ([part2 (generate-interleavings part1 (History-op-list history))])
      (display "part2 done\n")
      (let ([part3 (get-interleavings-as-lists part2)])
        (display "part3 done\n")
        (add-ids-to-instruction-lists part3)))))
  ;; (add-ids-to-instruction-lists (get-interleavings-as-lists (generate-interleavings (replace-calls client method-sketch) (History-op-list history)))))



;; (generate-simulation client-pre-example method-example)

;; We need a way to represent histories
(define-struct History (start-env op-list))
(define-struct Operation (op interval id))
(define-struct Interval (start end))




;; Get all sequential runs represented by a history
(define (get-seq-runs h)

  (define (extract-seq-runs o-list)
    (if (and (not (empty? o-list)) (> (length o-list) 1))
        (let ([first-o (first o-list)] [second-o (first (rest o-list))])
          (let ([first-i (Operation-interval first-o)] [second-i (Operation-interval second-o)])
              
            (cond
              [(<= (Interval-start second-i) (Interval-end first-i))
               (append

                (map (append-item (Instruction (Operation-op first-o) #f (Operation-id first-o) 0 0 #f (Empty))) (extract-seq-runs (rest o-list)))
                (map (append-item (Instruction (Operation-op second-o) #f (Operation-id second-o) 0   0 #f (Empty))) (extract-seq-runs (append (list first-o) (rest (rest o-list))))))] 
              [else (map (append-item (Instruction (Operation-op first-o) #f (Operation-id first-o) 0   0 #f (Empty))) (extract-seq-runs (rest o-list)))])))
        
        (cond
          [(empty? o-list) (list (list))]
          [else (list (list (Instruction (Operation-op (first o-list)) #f (Operation-id (first o-list)) 0 0   #f (Empty))))])))
       

  
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
  ;; (display "interested vals\n")
  ;; (display env-list) (display "\n")
  ;; (if (empty? (get-mapped (first env-list) "r0"))
  ;;     (begin (display (get-mapped (first env-list) "id")) (display "\n"))
  ;;     (void))
  
  (cond
    [(empty? env-list) (list (list))]
    [else
     (append (list (list (cons "r0" (get-mapped (first env-list) "r0")) (cons "z0" (get-mapped (first env-list) "z0")) (cons "possible"   (get-mapped (first env-list) "possible")) (list "id" (get-mapped (first env-list) "id")))) ;; (cons "id" (get-mapped (first env-list) "id"))))
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
   (Instruction `() #f 0 0 0 #f (Empty))
   (Instruction `() #f 1 0 0 #f (Empty))
   (Instruction `() #f 2 0 0 #f (Empty))))

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
                            [(= x 2) (list-remove e "l1" 0 "z6")])) #t 0 0 0 #f (Empty))

           (Instruction (lambda (e)
                          (cond
                            [(= y 0) (list-add e "l1" "z0")]
                            [(= y 1) (list-add e "l1" "z1")]
                            [(= y 2) (list-remove e "l1" 0 "z7")])) #t 0 0 0 #f (Empty)))))

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
        (Instruction (lambda (e) (list-add e "l1" "z5")) #f 1 0 0 #f (Empty))
        (Instruction (lambda (e) (list-remove e "l1" 4 "z6")) #f 2 0 0 #f (Empty)))))))))


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
        ;; (define (create-assertion-for id)
          ;; (display "Asserting ") (display (get-mapped env "id"))  (display "=>\n") (display env) (display "\n==") (display history-correct-envs) (display "\n Result: ") (display  (has-equivalent-env env history-correct-envs)) (display "\n")

          ;; (display "Asserting: ") (display (is-possible-env env)) (display "=>\n") (display env) (display "\n==") (display history-correct-envs) (display "\n Result: ") (display (has-equivalent-env env history-correct-envs)) (display "\n")
          ;; (display "asserting something\n") (display (or (not (is-possible-env env))) (has-equivalent-env env history-correct-envs)) (display "\n")

        (let ([check-var  (or (not (is-possible-env env)) (has-equivalent-env env history-correct-envs))])
            (display "-------") (display check-var) (display "-----------\n")
            (if (not check-var)
                (begin (display env)             (display "\n"))
                (void)

                )
          (assert check-var)))))


          ;; (assert (or (not (is-possible-env env)) (has-equivalent-env env history-correct-envs))))
          
          
        ;; (map create-assertion-for sim))))
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


(define (or-all l)
  (cond
    [(empty? l) #f]
    [else (if (first l)
              #t
              (or-all (rest l)))]))



(define (verify-sketch-with-history sim history-correct-envs generated-envs client sketch history)
  (let ([all-asserts (generate-invariant-assertions client sketch history sim)])
    ;; (display "finished all asserts\n")
    ;; (display (length generated-envs)) (display "\n")


    (assert (or-all (map is-possible-env generated-envs)))
    ;; (display (or (map is-possible-env generated-envs))) (display "\n\n\n\n")
    ;; (display generated-envs)
    (for-each
     all-asserts
     ;; (take generated-envs (- (length generated-envs) 80)))
     generated-envs)


    ))




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

(define conc-env (list (list "m1" (AMap-entry "A" 12 #f)) (cons "a1" "A")))

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
(define mystery 0)
(define mystery2 0)
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
        (Instruction (lambda (e) (amap-remove-arg e "m1" "a1" "z0")) #f 0 0 0 #f (Empty)))))))))
       

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
           (Instruction (lambda (e) ;;(display "line: 1\n")
                                (update-mapped e "val" "Null")) #t 1 mystery4 100 #f (Empty))
           (Instruction (lambda (e) ;;(display "line: 2\n")
                                (amap-contains-arg e "m1" "a1" "found")) #t 1 mystery3 101 #t (Meta-information "m1" "amap-contains-arg" "a1" "found" (Empty)))
           (Branch (lambda (e) (get-mapped e "found"))
                   (list 
                    (Instruction (lambda (e) ;;(display "line: 3\n")
                                         (amap-get-arg e "m1" "a1" "val")) #t 1 mystery2 102 #t (Meta-information "m1" "amap-get-arg" "a1" "val" (Empty)))
                    (Instruction  (lambda (e) ;;(display "line: 4\n")
                                          (amap-remove-arg e "m1" "a1" "forget")) #t 1 mystery 103 #t (Meta-information "m1" "amap-remove-arg" "a1" "forget" (Empty))))
                   (list))
           (Instruction (lambda (e)  ;;(display "line: 5\n")
                                (update-mapped e "r0" (get-mapped e "val"))) #t 1 mystery5 104 #f (Empty)))))

(define-struct Tuple (a b) #:transparent)



             
;; (add-optimistic concurrent-sketch)

(define-struct ERROR ())

(define (run-method-list instr-list env)
  ;; (display (get-mapped env "m1"))
  ;; (display (asserts)) (display "\n")
  (define (doloop condition instructions e depth)
    (let ([res (run-method-list instructions env)])
      (cond
        [(equal? depth 0) ;; (display "Loop unrolled too far, breaking out!\n")
         (ERROR)]
        [else
         (if (condition res) (begin ;;(display (get-mapped res "found")) (display "\n")
             (doloop condition instructions res (- depth 1)))
             res)])))

  (if (empty? instr-list)
      env
      (match (first instr-list)
        [(list) (run-method-list (rest instr-list) env)]
        [(Instruction f is-method id atomic inner-id rw? meta) (display "instruction-") (display inner-id) (display "\n")
         (run-method-list (rest instr-list) (f env))]
        [(Loop condition loop-contents)  (display "loop\n")
         (doloop condition loop-contents env 1)]
        [(Continue) (display "continue\n")
         env]
        [(Single-branch condition branch) ;; WARNING: This does not allow anything to happen after the branch succeeds.
         ;; Only continues after branch condition fails 
         (display "single-cond\n")
         (let ([cond-result (and (condition env) (boolean? (condition env)))])
           (if cond-result
               (let ([branch-result (run-method-list branch env)])
                 env)
                 ;; (run-method-list (rest instr-list) branch-result))
               (run-method-list (rest instr-list) env)))]
        [(Branch condition branch1 branch2) (display "branch\n")
         (let ([cond-result (and (condition env) (boolean? (condition env)))])

                                              (let ([branch-result (if cond-result (run-method-list branch1 env) (run-method-list branch2 env))])
                                                (run-method-list (rest instr-list) branch-result)))])))
        

(define (run-sequential-sketch-symbolic sketch test-env)
  (run-method-list (Method-instr-list sketch) test-env))
    
(define-symbolic a1 integer?)
(define-symbolic e1 e2 e3 integer?)


(define seq-test-env (list (list "m1" (AMap-entry e2 e1 #f) (AMap-entry 1 7 #f)) (cons "a1" a1)))
;; (run-sequential-sketch-symbolic (add-optimistic concurrent-sketch) seq-test-env)

;; (let ([test-env (list (list "m1" (AMap-entry e2 e1) (AMap-entry 1 7)) (cons "a1" a1))])
  ;; (synthesize #:forall (list)
  ;;             #:guarantee (assert (equivalent-envs? (run-sequential-sketch-symbolic (Tuple-b (add-optimistic concurrent-sketch)) test-env) (reference test-env))))


(define-symbolic* test-sym integer?)
(define concurrent-sketch-optim
  (Method "removeAttribute"
          (list
           (Instruction (lambda (e) (update-mapped e "optim" #t)) #t 1 0 99 #f (Empty))
           (Loop (lambda (e) (get-mapped e "optim"))
                 (list 
                  (Instruction (lambda (e) (update-mapped e "val" "Null")) #t 1 mystery4 100 #f (Empty))
                  (Instruction (lambda (e) (amap-contains-arg e "m1" "a1" "found")) #t 1 mystery3 101 #f (Empty))
                  (Branch (lambda (e) (get-mapped e "found"))
                          (list 
                           (if (= test-sym 0)
                               (Branch (choose* (lambda (e) (get-mapped e "found")) (lambda (e) (get-mapped e "z0")) )
                                       (list
                                        (Instruction (lambda (e) (amap-get-arg e "m1" "a1" "val")) #t 1 mystery2 102 #f (Empty))
                                        (Instruction (lambda (e) (update-mapped e "optim" #f)) #t 1 0 98 #f (Empty)))
                                       (list))

                                (Branch (lambda (e) #t)
                                        (list (Instruction (lambda (e) (amap-get-arg e "m1" "a1" "val")) #t 1 mystery2 102 #f (Empty)))
                                        (list))
                                )

                               ;; (Instruction (lambda (e) (amap-get-arg e "m1" "a1" "val")) #t 1 mystery2 102))
                               ;; (Instruction (lambda (e) (update-mapped e "optim" #f)) #t 1 0 98)
                               ;; (Instruction (lambda (e) e)))
                           

                           (Instruction (lambda (e) (amap-remove-arg e "m1" "a1" "forget")) #t 1 mystery 103 #f (Empty)))
                   `())
                   
                    

                 (Instruction (lambda (e) (update-mapped e "r0" (get-mapped e "val"))) #t 1 mystery5 104 #f (Empty)))))))





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



    
(define (add-optimistic-helper instr-list m m-num)
  ;; (define-symbolic* m integer?)

  ;; (define m 0)

  (cond 
    [(empty? instr-list) (Tuple `() `())]
    [(Branch? (first instr-list)) 
     (let ([condition (Branch-condition (first instr-list))]
           [branch1 (Branch-branch1 (first instr-list))]
           [branch2 (Branch-branch2 (first instr-list))])
       (let ([b1-res (add-optimistic-helper branch1 m (+ m-num 1))]
             [b2-res (add-optimistic-helper branch2 m (+ m-num 1))]
             [rec-result (add-optimistic-helper (rest instr-list) m (+ m-num 5))])
         (Tuple
          (append (Tuple-a b1-res) (Tuple-a b2-res) (Tuple-a rec-result))
          (append
           (list (Branch condition (Tuple-b b1-res) (Tuple-b b2-res)))
           (Tuple-b rec-result)))))]
    [else

     ;; (display m) (display "-") (display "line: ")
     ;; (if (Instruction? (first instr-list)) (begin
     ;;                                         (if (equal? (Instruction-inner-id (first instr-list)) 103)
     ;;                                             (assert (= m 1))
     ;;                                             (void))
     ;;                                         (display (Instruction-inner-id (first instr-list))) (display "\n")) (void))

     (let ([rec-result (add-optimistic-helper (rest instr-list) m (+ m-num 1))])
       (Tuple
        (append (list m) (Tuple-a rec-result))
        (append
         (list (Branch (lambda (e)  (equal? m m-num))
                       (list (Single-branch
                              (choose (lambda (e) #t) (lambda (e) (not (get-mapped e "found"))))
                              (list
                               (Continue)))
                             (Instruction (lambda (e) (update-mapped e "optim" #f)) #t 1 0 98 #f 0))
                       (list)))
         (append (list (first instr-list)) (Tuple-b rec-result)))))]))




(define (add-optimistic sketch)
  (define-symbolic* m integer?)
  (let ([instr-list (Method-instr-list sketch)] [name (Method-id sketch)])
    (let ([opt-helper-res (add-optimistic-helper instr-list m 0)])
      
      
      (display (first (Tuple-a opt-helper-res))) (display "\n")
      (Method
       name
       (append
        (list (Instruction (lambda (e) 
                             (update-mapped e "optim" #t)) #t 1 0 105 #f (Empty)))
        (list (Loop (lambda (e) (get-mapped e "optim"))                 
                    (Tuple-b opt-helper-res))))))))



(define test-env (list (list "m1" (AMap-entry 4 "A" #f) (AMap-entry 1 7 #f)) (cons "a1" 4) (cons "found" #f)))
;; (get-mapped (run-sequential-sketch-symbolic (add-announcements concurrent-sketch) test-env) "r0")


(define simple-concurrent
  (Method "blah"
          (list
           (Instruction (lambda (e) (update-mapped e "found" #f)) #f 0 0 0 0 0)
           (Instruction (lambda (e) (update-mapped e "x" "y")) #f 1 0 0 0 0))))


(define (cex-guided-repair-cycle client sketch history)
        
        (define (loop client sketch history)
          (let ([sim (generate-simulation client sketch history)])
            (display "done sim\n")
            ;; ;; (display (first (interested-vals (run-lists sim (History-start-env history))))) (display "\n\n\n")
            (let ([env-list (filter
                             (lambda (e) (not (empty? e)))
                             (interested-vals (run-lists sim (History-start-env history))))])
              ;; (map (lambda (e) (display e) (display "\n")) env-list))))
              (let ([history-correct-envs (valid-environments history)])
              
            ;;     ;; (display history-correct-envs) (display "\n")


            ;;     ;; (display (get-mapped (second env-list) "possible")) (display "\n")
                (let ([binding (synthesize #:forall (list)
                                           #:guarantee (verify-sketch-with-history sim history-correct-envs env-list client sketch history))])
                  binding)))))
                ;; (map (lambda (e) (display e) (display "\n")) env-list)))))



        (loop client sketch history))
  
;; (map
;;  (lambda (l)
;;    (has-equivalent-env l (valid-environments concurrent-history)))


;; ADD OPTIMISTIC
;; (time (cex-guided-repair-cycle concurrent-client    (add-optimistic concurrent-sketch) concurrent-history))



;; (asserts)
;; (map (lambda (e) (display e) (display "\n")) (cex-guided-repair-cycle concurrent-client concurrent-sketch concurrent-history))

 

;; (amap-remove-arg e "m1" "a1" "z0")

  ;; (generate-simulation concurrent-client (add-optimistic concurrent-sketch) concurrent-history)


(define (is-rw-call? instr)
  (cond
    [(Instruction? instr) (Instruction-rw? instr)]
    [else #f]))


(define (announce-amap e name key)
  (let ([old-amap (get-mapped e name)])
    (let ([new-amap (map
                     (lambda (p) (if (equal? key (AMap-entry-key p))
                                     (AMap-entry (AMap-entry-key p) (AMap-entry-val p) #t)
                                     p))
                     old-amap)])
      (update-mapped e name new-amap))))


;; Generates an announcement instruction which marks that no one can interfere with the results of the instruction
;; for the duration of the caller method's lifetime
(define (Make-announcement instr)
  (let ([meta-info (Instruction-meta instr)])
    (let ([method (Meta-information-method meta-info)]
          [obj (Meta-information-obj meta-info)]
          [arg1 (Meta-information-arg1 meta-info)]
          [arg2 (Meta-information-arg2 meta-info)]
          [arg3 (Meta-information-arg3 meta-info)])
      (cond
        [(equal? method "amap-contains") 
         (Instruction (lambda (e)  (announce-amap e obj arg1))  #t 1 0 107 #f (Empty))]
        [(equal? method "amap-remove")
         (Instruction (lambda (e) (update-mapped (announce-amap e obj arg1) "BOOP" 1)) #t 1 0 107 #f (Empty))]
        [(equal? method "amap-get")
         (Instruction (lambda (e) (announce-amap e obj arg1)) #t 1 0 107 #f (Empty))]
        [(equal? method "amap-put")
         (Instruction (lambda (e) (announce-amap e obj arg1)) #t 1 0 107 #f (Empty))]

        [(equal? method "amap-contains-arg") 
         (Instruction (lambda (e)  (announce-amap e obj (get-mapped e arg1)))  #t 1 1 107 #f (Empty))]
        [(equal? method "amap-remove-arg")
         (Instruction (lambda (e) (update-mapped (announce-amap e obj (get-mapped e arg1)) "BOOP" 1)) #t 1 1 107 #f (Empty))]
        [(equal? method "amap-get-arg")
         (Instruction (lambda (e) (announce-amap e obj (get-mapped e arg1))) #t 1 1 107 #f (Empty))]
        [(equal? method "amap-put-arg")
         (Instruction (lambda (e) (announce-amap e obj (get-mapped e arg1))) #t 1 1 107 #f (Empty))]
        [(equal? method "amap-putIfAbsent-arg")
         (Instruction (lambda (e) (announce-amap e obj (get-mapped e arg1))) #t 1 0 107 #f (Empty))]))))
    




(define (add-announcements sketch)  

  (define (add-announcements-helper instr-list do-announcement? tracker)

    ;; (display (asserts)) (display "\n")
    ;; (display instr-list) (display "-") (display do-announcement?)
    ;; (display "\n")

    (cond
      [(empty? instr-list) `()]
      [(Branch? (first instr-list))
       (let ([condition (Branch-condition (first instr-list))]
             [branch1 (Branch-branch1 (first instr-list))]
             [branch2 (Branch-branch2 (first instr-list))])
         (let ([b1-res (add-announcements-helper branch1 do-announcement? tracker)]
               [b2-res (add-announcements-helper branch2 do-announcement? tracker)]
               [rec-result (add-announcements-helper (rest instr-list) do-announcement? (+ tracker 5))])
           (append
            (list (Branch condition b1-res b2-res))
            rec-result)))]
      [(is-rw-call? (first instr-list))
       (display "tracker: ") (display tracker)  (display "-") (display (Instruction-inner-id (first instr-list))) (display "\n")
       (let ([rec-result (add-announcements-helper (rest instr-list) do-announcement? (+ tracker 1))])
         
         (list (Meta-branch (lambda (e) (equal? do-announcement? tracker))                         
                       (append
                        (list (Make-announcement (first instr-list))
                              (first instr-list))
                        rec-result)
                       (append (list (first instr-list)) rec-result))))]
               
      ;; [(and (equal? do-announcement? 1) (is-rw-call? (first instr-list)))
      ;;  (display (Instruction-inner-id (first instr-list))) (display "\n")
      ;;  (let ([rec-result (add-announcements-helper (rest instr-list))])
      ;;    (append
      ;;     (list (Make-announcement (first instr-list))
      ;;           (first instr-list))
      ;;     rec-result))]


      [else
       (append (list (first instr-list)) (add-announcements-helper (rest instr-list) do-announcement? tracker))]))

  

  
  (let ([instr-list (Method-instr-list sketch)] [name (Method-id sketch)])
    (define-symbolic* do-announcement? integer?)
    ;; (assert (= do-announcement? 3))
    (Method
     name
     (add-announcements-helper instr-list do-announcement? 0))))


(define (test-announcements-reference e)
  (let ([ret-env (amap-put e "m1" "bleep" "bloop")])
    (amap-get ret-env "m1" "bleep" "r0")))


(define test-announcements-env
  (list (list "m1")))


(define-symbolic achoice1 integer?)
(define-symbolic achoice2 integer?)

(define test-announcements-sketch
  (Method
   "test"
   (list
    (Meta-branch (lambda (e) (equal? achoice1 0))
            (list 
            (Make-announcement (Instruction (lambda (e) (amap-put e "m1" "bleep" "bloop")) #t 1 0 100 #t
                                            (Meta-information "m1" "amap-put" "bleep" "bloop" (Empty))))
            (Instruction (lambda (e) (display "achoice1 is 0\n") e) #t 1 0 500 #t (Empty))
            (Instruction (lambda (e) (amap-put e "m1" "bleep" "bloop")) #t 1 0 100 #t (Empty)))
            (list (Instruction (lambda (e) (amap-put e "m1" "bleep" "bloop")) #t 1 0 100 #t (Empty)))))))


    ;; (Meta-branch (lambda (e) (equal? achoice1 1))
    ;;         (list
    ;;          (Make-announcement (Instruction (lambda (e) (amap-get e "m1" "bleep" "r0")) #t 1 0 101 #t
    ;;                                          (Meta-information "m1" "amap-get" "bleep" "r0" (Empty))))
    ;;          (Instruction (lambda (e) (amap-get e "m1" "bleep" "r0")) #t 1 0 101 #t (Empty)))
            
    ;;           (list (Instruction (lambda (e) (amap-get e "m1" "bleep" "r0")) #t 1 0 101 #t (Empty)))))))

(define test-announcements-client
         (Client-pre
          (list 
           (Thread-list
            (list
             (Client-pre
              (list
               (Sketch-placeholder "test")))
             (Client-pre
              (list
               (Instruction (lambda (e) (amap-remove e "m1" "bleep" "z0")) #t 0 0 0 #f (Empty)))))))))
         
(define test-announcements-history
         (History test-announcements-env
                  (list
                   (Operation (lambda (e) (amap-remove e "m1" "bleep" "z0")) (Interval 0 2) 0)
                   (Operation (lambda (e) (test-announcements-reference e)) (Interval 1 3) 1))))







(define (fix-holes-with-announcement sketch holes)
  (define-struct Hole-result (sym-vars instr-list looking?))
  ;; TODO - does not handle case of hole between branch and rest of program
  (define (plug-hole-with-announcement instr-list hole looking?)
    (define-symbolic* choice integer?)



      (cond
        [(empty? instr-list)
         (Hole-result `() instr-list #f)]
        [(Branch? (first instr-list))
         (let ([recursive-result (plug-hole-with-announcement (rest instr-list) hole looking?)])
           (let ([condition (Branch-condition (first instr-list))]
                 [branch1 (Branch-branch1 (first instr-list))]
                 [branch2 (Branch-branch2 (first instr-list))])
             
             (let ([branch1-result (plug-hole-with-announcement branch1 hole (Hole-result-looking? recursive-result))]
                   [branch2-result (plug-hole-with-announcement branch2 hole (Hole-result-looking? recursive-result))])
             
               (Hole-result
                (append (Hole-result-sym-vars branch1-result) (Hole-result-sym-vars branch2-result) (Hole-result-sym-vars recursive-result))
              
              
              (append
               (list (Branch condition
                             (Hole-result-instr-list branch1-result)
                             (Hole-result-instr-list branch2-result)))
              (Hole-result-instr-list recursive-result))
              
              (or (Hole-result-looking? branch1-result) (Hole-result-looking? branch2-result))))))]
        [(Instruction? (first instr-list))
         (let ([recursive-result (plug-hole-with-announcement (rest instr-list) hole looking?)])
           (cond
             [(and (or looking? (Hole-result-looking? recursive-result))
                   (is-rw-call? (first instr-list)))
              (display (Instruction-inner-id (first instr-list))) (display "\n")
              (Hole-result 
               (append (list choice) (Hole-result-sym-vars recursive-result))
               
               (append
                (list
                 (Meta-branch (lambda (e) (= choice 1))
                              (list
                               (Make-announcement (first instr-list))
                               (first instr-list))
                              (list (first instr-list))))
                (Hole-result-instr-list recursive-result))
               #f)]
            
             
           [(equal? (Instruction-inner-id (first instr-list)) (Hole-method1 hole))
            (Hole-result (Hole-result-sym-vars recursive-result)
                         (append (list (first instr-list)) (Hole-result-instr-list recursive-result))
                         #t)]
           [else
            (Hole-result
             (Hole-result-sym-vars recursive-result)
             (append (list (first instr-list)) (Hole-result-instr-list recursive-result))
             looking?)]))]))

       
          
          

       
                
                        
         
      


  
  (cond
    [(empty? holes) (Tuple `() sketch)]
    [else
     (let ([repair-sketch-tuple (plug-hole-with-announcement (Method-instr-list sketch) (first holes) #f)])
       (let ([new-symbols (Hole-result-sym-vars repair-sketch-tuple)] [new-sketch (Hole-result-instr-list repair-sketch-tuple)]) 
         (let ([recurse-result (fix-holes-with-announcement (Method (Method-id sketch) new-sketch) (rest holes))])
           (Tuple (append new-symbols (Tuple-a recurse-result)) (Tuple-b recurse-result)))))]))
         


(fix-holes-with-announcement concurrent-sketch (list (Hole 104 0 103)))
;; (cex-guided-repair-cycle test-announcements-client test-announcements-sketch test-announcements-history)
;; (cex-guided-repair-cycle concurrent-client (add-optimistic concurrent-sketch) concurrent-history)




          


            
             




;; (time (cex-guided-repair-cycle concurrent-client (add-announcements concurrent-sketch) concurrent-history))
                


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



