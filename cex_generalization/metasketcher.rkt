#lang racket
(require "../program_representation/simulator-structures.rkt")
(require "../utilities/utilities.rkt")

(provide
 metasketch-announcement-strategy
 metasketch-library-add-announcement
 expand-traces-to-sketch-lib
 metasketch-optimistic-strategy
 modify-library-for-optimistic
 meta-vars-consistent?
 collect-all-optimistic-expressions
 optimistic-stopped-hole?
 minimal-lock
 )

(define optimistic-count (void))
(set! optimistic-count 0)
(define (new-optimistic-id)
  (set! optimistic-count (+ optimistic-count 1))
  optimistic-count)









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; Metasketch for Base Library ;;;;;;;;;;;;;;;;;;;;;;
(define (metasketch-library-add-announcement library method-name)

  ;; We are able to make an announcement any time before
  ;; the Lin. point of the method
  (define (repair-lib-announce-helper instr-list meta-var)
    (cond
      [(empty? instr-list)
       ;; (display "Found empty: ")
       ;; (displayln instr-list)
       `()]

      [(Loop? (first instr-list))
       ;; (displayln "Found a loop")
       (if (and
            (not (has-write-op? (Loop-instr-list (first instr-list))))
            (has-read-op? (Loop-instr-list (first instr-list))))

           (let ([pointer-to-read-obj (get-read-pointer (Loop-instr-list (first instr-list)))])
             ;; (display "Found the following pointer: ")
             ;; (displayln pointer-to-read-obj)
             (append
              (list (first instr-list))
              (list (Meta-addition-choice
                     (list (Set-pointer (car pointer-to-read-obj) (cdr pointer-to-read-obj) "bits" (Mystery-const)))
                     meta-var))
              (repair-lib-announce-helper (rest instr-list) meta-var)))

           (append
            (list (first instr-list))
            (repair-lib-announce-helper (rest instr-list) meta-var)))]


      [(Single-branch? (first instr-list))

       (let ([saw-write-op? (has-write-op? (Single-branch-branch (first instr-list)))])
         (append
          (list
           (Single-branch
            (Single-branch-condition (first instr-list))
            (repair-lib-announce-helper (Single-branch-branch (first instr-list)) meta-var)))
          (if saw-write-op?
              (rest instr-list)
              (repair-lib-announce-helper (rest instr-list) meta-var))))]

          
      [(Branch? (first instr-list))

       (let ([saw-write-op?
              (or
               (has-write-op? (Branch-branch1 (first instr-list)))
               (has-write-op? (Branch-branch2 (first instr-list))))])

         (append
          (list
           (Branch
            (Branch-condition (first instr-list))

            (repair-lib-announce-helper (Branch-branch1 (first instr-list)) meta-var)
            (repair-lib-announce-helper (Branch-branch2 (first instr-list)) meta-var)))

          (if saw-write-op?
              (rest instr-list)
              (repair-lib-announce-helper (rest instr-list) meta-var))))]

      

      
      [(read-operation? (first instr-list))
       ;; (displayln "Found read op")
       (let ([pointer-to-read-obj (read-operation-pointer (first instr-list))])

         (append
          (list
           (Meta-addition-choice
            (list (Set-pointer (car pointer-to-read-obj) (cdr pointer-to-read-obj) "bits" (Mystery-const)))
            meta-var))
            
          (list (first instr-list))

          (repair-lib-announce-helper (rest instr-list) meta-var)))]

      [(write-operation? (first instr-list))
       ;; (displayln "Found write op")
       (rest instr-list)]
      [else
       ;; (display "Not read or write: ")
       ;; (displayln (first instr-list))
       (append
        (list (first instr-list))
        (repair-lib-announce-helper (rest instr-list) meta-var))]
      ))
          
    
    


  (let
      ([methods-of-interest
        (filter
         (lambda (m)
           (equal? (Method-id m) method-name))
         library)]
       [rest-of-methods
         (filter
          (lambda (m)
            (not (equal? (Method-id m) method-name)))
          library)])

    (if (or (> (length methods-of-interest) 1) (< (length methods-of-interest) 1))
        (displayln "WARNING: Found strange number of library matches for given name. Probably an error...")

        (let
            ([method-match (first methods-of-interest)])
          ;; (display "Method-matched: ")
          ;; (displayln (repair-lib-announce-helper (Method-instr-list method-match) (new-meta-var)))
          (append
           (list
            (Method
             (Method-id method-match)
             (Method-args method-match)
             (Method-ret-type method-match)
             (repair-lib-announce-helper (Method-instr-list method-match) (new-meta-var))))

            
           rest-of-methods)))))

          
          


(define (add-start-label instr-list)
  (cond
    [(empty? instr-list) `()]
    [(Create-var? (first instr-list)) (append (list (first instr-list)) (add-start-label (rest instr-list)))]
    [else
     (append (list (Label "START")) instr-list)]))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Metasketch for Composed Method ;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (modify-library-for-optimistic library method-name hole)
  (let ([matching-method (get-lib-method library method-name)])
    (cond
      [(null? matching-method)
       (displayln "Warning: Asked to modify a library method that doesn't exist. This is probably a mistake")]
      [else
       (let ([fixed-method (metasketch-optimistic-strategy (Method-instr-list matching-method)
                                                           hole)])
         (let ([fixed-method-with-start-label (add-start-label fixed-method)])
           (replace-lib-method library method-name
                               (Method (Method-id matching-method) (Method-args matching-method) (Method-ret-type matching-method) fixed-method-with-start-label))))])))



;;;;;;;; Optimistic Concurrency repair protocol ;;;;;;;;;;;;;;;;;
(define (metasketch-optimistic-strategy instr-list hole)
  (metasketch-repair instr-list optimistic-repair-strt optimistic-repair-end hole (new-meta-var)))

  



;; TODO: Have to actually interpret Optimistic-Condition
(define (generate-optimistic-check-expression interruptor output-var)
  (Set-var output-var (Optimistic-Condition (new-optimistic-id))))

  


(define (optimistic-repair-strt instr-list hole meta-var)
  (list

   (Meta-branch
    meta-var
    (list
     (first instr-list)
     ;; (Atomic-Start-Marker)
     (generate-optimistic-check-expression (Hole-interruptor hole) "OPTIMISTIC")
     (Single-branch-counter (Get-var "OPTIMISTIC")
                    (list
                     (Goto "START" 0))

                    0))


    (list (first instr-list)))))
    


(define (optimistic-repair-end instr-list hole meta-var)
  (list (first instr-list)))
;; (define (optimistic-repair-end instr-list hole meta-var)

  
;;   (append

   
;;    (list
;;    (Meta-branch
;;     meta-var
;;     (list
;;      (Atomic-End-Marker))
;;     `()))

;;    instr-list))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;; Announcement repair protocol ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (metasketch-announcement-strategy instr-list hole)
  (metasketch-repair instr-list announcement-repair-strt announcement-repair-end hole (new-meta-var)))


;; TODO: Decide how to check for an announcement that's been made
(define (generate-announcement-check interruptor output-var)
  (Set-var output-var "ANNOUNCEMENT"))

;; Tries to defend a given instr-list from a particular hole happening using
;; announcements
;; modify the base library
(define (announcement-repair-strt instr-list hole meta-var)
  (list

   (Meta-branch
    meta-var
   (list
    (first instr-list)
    (Atomic-Start-Marker)
    (generate-announcement-check (Hole-interruptor hole) "ANNOUNCE")
    (Single-branch (Get-var "ANNOUNCE")
                   (list
                    (Goto  "START")))
    )

   (list (first instr-list)))))

(define (announcement-repair-end instr-list hole meta-var)
  (append
   (list
    (first (instr-list))
    (Meta-branch
     meta-var
     (list
      (Atomic-End-Marker))
     `()))))
   






;; Metasketcher takes as input an instr-list representing a composed method
;; as well as two functions, repair-strt and repair-end. These are the two
;; functions that define the repair that will be done. Repair functions take
;; the remaining instr-list, as well as the hole
(define (metasketch-repair instr-list repair-strt repair-end hole meta-var)

  ;; This checks to see if we have entered a hole and have not exited at the end of the branch
  (define (check-branch-border b-instr-list)
    ;; TODO
    #f)

  ;; Checks to see if the loop contains the end point of a hole that starts later in the loop
  ;; - this means that we don't have to close the hole later on
  (define (check-loop-first loop-instr-list)
    ;; TODO
    #f)

  
  (cond
    [(empty? instr-list) instr-list]

    [(Single-branch? (first instr-list))

     (let ([branch-ends-with-hole? (check-branch-border (Single-branch-branch (first instr-list)))])
       (append
        (list
         (Single-branch
          ;; (C-Instruction-instr-id (first instr-list))

          (Single-branch-condition (first instr-list))
          (metasketch-repair (Single-branch-branch (first instr-list)) repair-strt repair-end hole meta-var)))

        (if branch-ends-with-hole?
            (repair-end (rest instr-list) hole)
            (metasketch-repair (rest instr-list) repair-strt repair-end hole meta-var))))]
    
    [(Branch? (first instr-list))
     ;; Check to see if one of the branches ends with the beginning of the hole
     ;; in which case we will need to apply repair-end after the body of the Branch
     (let ([branch-ends-with-hole? (or
                                    (check-branch-border (Branch-branch1 (first instr-list)))
                                    (check-branch-border (Branch-branch2 (first instr-list))))])

       ;; Recursively perform repair
       (append
        (list
         (Branch
          ;; (C-Instruction-instr-id (first instr-list))
          
          (Branch-condition (first instr-list))
          (metasketch-repair 
           (Branch-branch1 (first instr-list)) repair-strt repair-end hole meta-var)
          (metasketch-repair
           (Branch-branch2 (first instr-list)) repair-strt repair-end hole meta-var)))
        
        ;; We may or may not have to complete the repair after the branch
        (if branch-ends-with-hole?
            (repair-end (rest instr-list) hole)
            (metasketch-repair (rest instr-list) repair-strt repair-end hole meta-var))))]

    [(Loop? (first instr-list))
     ;; Check to see if the loop body ends with the start of the hole - in which case
     ;; we must close up the hole either at the start of the loop body, or after the
     ;; lop has ended
     (let ([ends-with-hole? (check-branch-border (Loop-instr-list (first instr-list)))]
           [loop-first-completes-hole? (check-loop-first (Loop-instr-list (first instr-list)))])



       (cond
         ;; As long as the start of the loop doesn't complete the hole, we can proceed as with the
         ;; Branch case
         [(not loop-first-completes-hole?)
          (append
           (list
           (Loop
            ;; (C-Instruction-instr-id (first instr-list))
            
            (Loop-condition (first instr-list))
            (metasketch-repair (Loop-instr-list (first instr-list)) repair-strt repair-end hole meta-var)))

           (if ends-with-hole?
               (repair-end (rest instr-list) hole)
               (metasketch-repair (rest instr-list) repair-strt repair-end hole meta-var)))]

         ;; If the start of the loop completes the hole, then we have to close up the hole
         [(and loop-first-completes-hole? ends-with-hole?)
          (let ([loop-body (Loop-instr-list (first instr-list))])
            (append
             (list
              (Loop
               ;; (C-Instruction-instr-id (first instr-list))

               (Loop-condition (first instr-list))

               (repair-end loop-body hole)))

             (repair-end (rest instr-list hole))))]


          

         ;; Otherwise we can just be safe in recursing our repair into the loop body
         [else
          (let ([loop-body (Loop-instr-list (first instr-list))])
            (append
             (list
              (Loop
               ;; (C-Instruction-instr-id (first instr-list))
               
               (Loop-condition (first instr-list))
               (metasketch-repair loop-body repair-strt repair-end hole meta-var)))
             
             (metasketch-repair (rest instr-list) repair-strt repair-end hole meta-var)))]))]
            

           
    [(Label? (first instr-list))
     (append
      (list (first instr-list))
      (metasketch-repair (rest instr-list) repair-strt repair-end hole meta-var))]
       
       
    [(Meta-branch? (first instr-list)) ;; TODO: For now we assume that the hole can't be in a previously patched section
     ;;                                         so we actually ignore Meta-branches

     (append (list (first instr-list)) (metasketch-repair (rest instr-list) repair-strt repair-end hole meta-var))]
         
      
    [(equal? (C-Instruction-instr-id (first instr-list)) (Hole-method1 hole)) ;; We've found our hole
     ;; (displayln "Found hole")
     (append
      (repair-strt instr-list hole meta-var) ;; Add whatever needs to be added to the first instruction
      (metasketch-repair (rest instr-list) repair-strt repair-end hole meta-var))]

    [(and (not (null? (Hole-method2 hole)))
          (equal? (C-Instruction-instr-id (first instr-list)) (Hole-method2 hole)))
     ;; (displayln "Found end of hole")
     (append
      (repair-end instr-list hole meta-var)
      (metasketch-repair (rest instr-list) repair-strt repair-end hole meta-var))]

    
    [else
     ;; (display "didn't find hole, found: ") (displayln (C-Instruction-instr-id (first instr-list)))
     (append (list (first instr-list))
     (metasketch-repair (rest instr-list) repair-strt repair-end hole meta-var))
     ]))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Expansion of error traces to match new sketch extension ;;;;;;;;;;

(define (expand-traces-to-sketch-lib traces library composed-method-name)
  ;; (display "traces: ")
  ;; (displayln traces)
  (let ([composed-sketch-methods
         (filter (lambda (m) (equal? (Method-id m) composed-method-name)) library)])
    (if (empty? composed-sketch-methods)
        (displayln "ERROR: You've asked for a method that does not exist in the library")

        (let ([composed-sketch-method (first composed-sketch-methods)])
          ;; (display "COMPOSED METHOD FOUND: ") (displayln composed-sketch-method)
          (reduce
           append
           (map (lambda (t)
                  (set-all-counters-to-zero (Method-instr-list composed-sketch-method))
                  (expand-error-trace t composed-sketch-method)) traces))))))
  




(define (set-all-counters-to-zero instr-list)
  (cond
    [(empty? instr-list) instr-list]

    [(Single-branch? (first instr-list))
     (set-all-counters-to-zero (Single-branch-branch (first instr-list)))
     (set-all-counters-to-zero (rest instr-list))]


    
    [(Branch? (first instr-list))
     (set-all-counters-to-zero (Branch-branch1 (first instr-list)))
     (set-all-counters-to-zero (Branch-branch2 (first instr-list)))
     (set-all-counters-to-zero (rest instr-list))]

    [(Loop? (first instr-list))
     (set-all-counters-to-zero (Loop-instr-list (first instr-list)))
     (set-all-counters-to-zero (rest instr-list))]
    
            

           
    [(Label? (first instr-list))
     (set-all-counters-to-zero (rest instr-list))]
       
       
    [(Meta-branch? (first instr-list))
     (set-all-counters-to-zero (Meta-branch-branch1 (first instr-list)))
     (set-all-counters-to-zero (Meta-branch-branch2 (first instr-list)))
     (set-all-counters-to-zero (rest instr-list))]


    [(Single-branch-counter? (first instr-list))
     (set-Single-branch-counter-counter! (first instr-list) 0)
     (set-all-counters-to-zero (Single-branch-counter-branch (first instr-list)))
     (set-all-counters-to-zero (rest instr-list))]
    
    [else
     (set-all-counters-to-zero (rest instr-list))]))
         




;; Takes a trace t and a method m and returns the set of traces equivalent to
;; t that are runnable using m 
(define (expand-error-trace original-t m )

  ;; (displayln "Expanding error trace...") (displayln original-t) 

  
  ;; Note: Keeps running list of all interrupting method calls
  ;; we see along the way, so that when we do restarts, we don't repeat the same calls
  (define (expand-error-trace-helper t instr-list interceptors-seen)
    ;; (display "FIRST: ")
    ;; (displayln (first t))

    ;; (display "Method: ") (displayln instr-list)
    ;; (display "trace: ") (displayln t)

    
    (cond
      [(or (empty? t) (empty? instr-list))
       (list (list))]

      [(Meta-branch? (first instr-list))
       ;; (displayln "Reached meta-branch")
       ;; (display "branch1: ")
       ;; (displayln (append (Meta-branch-branch1 (first instr-list)) (rest instr-list)))


       (append
        (map
         (append-item (Assume-meta (Meta-branch-condition (first instr-list))))
         (expand-error-trace-helper t (append (Meta-branch-branch1 (first instr-list)) (rest instr-list)) interceptors-seen))
        (map
         (append-item (Assume-not-meta  (Meta-branch-condition (first instr-list))))
         (expand-error-trace-helper t (append (Meta-branch-branch2 (first instr-list)) (rest instr-list)) interceptors-seen)))]




      [(Single-branch-counter? (first instr-list))
       ;; (display "Counter: ") (display (Single-branch-counter-counter (first instr-list))) (display "- ")
       ;; (displayln "Reached single branch ")
       (cond


         [(>= (Single-branch-counter-counter (first instr-list)) 1)

          (map
           (lambda (l)
             (append (list (Trace-Type "broke-out")
                           (Assume-simulation (Not (Single-branch-counter-condition (first instr-list))) #f)) l))


           (expand-error-trace-helper t (rest instr-list) interceptors-seen))]
          
         [else
          (set-Single-branch-counter-counter! (first instr-list) (+ (Single-branch-counter-counter (first instr-list)) 1))
          (append
           (map 
            ;; (append-item (Assume-simulation (Single-branch-counter-condition (first instr-list)) #f))
            (lambda (l)
              (append (list (Trace-Type "optimistic-restart")
                            (Assume-simulation (Single-branch-counter-condition (first instr-list)) #f))
                      l))
            (expand-error-trace-helper t (append (Single-branch-counter-branch (first instr-list))) interceptors-seen))

           (map
            ;; (append-item (Assume-simulation (Not (Single-branch-counter-condition (first instr-list))) #f))
            (lambda (l)
              (append (list (Trace-Type "no-optimistic-restart")
                            (Assume-simulation (Not (Single-branch-counter-condition (first instr-list))) #f))
                      l))
            (expand-error-trace-helper t (rest instr-list) interceptors-seen)))])]

           
         
          
         
         ;; ;; If in the original trace, this condition failed - it still fails
         ;; [(and (Assume-simulation? (first t)) (Not? (Assume-simulation-condition (first t))))
         ;;  (displayln "Assume - NOT")
         ;;  (map
         ;;   (append-item (first t))
         ;;   (expand-error-trace-helper (rest t) (rest instr-list) interceptors-seen))]

         ;; ;; If in the original trace, this condition succeeded- it succeeds
         ;; [(and (Assume-simulation? (first t)) (not (Not? (Assume-simulation-condition (first t)))))
         ;;  (displayln "Assume - NOT NOT")
         ;;  (map
         ;;   (append-item (first t))
         ;;   (expand-error-trace-helper (rest t) (append (Single-branch-counter-branch (first instr-list)) (rest instr-list))
         ;;                              interceptors-seen))]

         ;; ;; If this was not a condition that appeared in the original interleaving
         ;; ;; - we must explore both branches
         ;; [(not (Assume-simulation? (first t)))
         ;;  (displayln "Other assume")
         ;;  (append
         ;;   (map 
         ;;    (append-item (Assume-simulation (Single-branch-counter-condition (first instr-list))))
         ;;    (expand-error-trace-helper t (append (Single-branch-counter-branch (first instr-list))) interceptors-seen))

         ;;   (map
         ;;    (append-item (Assume-simulation (Not (Single-branch-counter-condition (first instr-list)))))
         ;;    (expand-error-trace-helper t (rest instr-list) interceptors-seen)))])]




      ;; If the composed method has a real program branch, then we need to figure out
      ;; if the branch is from the original error trace, in which case we must follow
      ;; the branch taken by the original program
      [(Single-branch? (first instr-list))
       ;; (displayln "Reached single branch")
       (cond



         
         ;; If in the original trace, this condition failed - it still fails
         [(and (Assume-simulation? (first t))
               (Not? (Assume-simulation-condition (first t)))
               (not (Assume-simulation-seen-before (first t))))
                                                       
          (set-Assume-simulation-seen-before! (first t) #t)
          ;; (displayln "Assume - NOT")
          (map
           (append-item (first t))
           (expand-error-trace-helper (rest t) (rest instr-list) interceptors-seen))]

         ;; If in the original trace, this condition succeeded- it succeeds
         [(and
           (Assume-simulation? (first t))
           (not (Assume-simulation-seen-before (first t)))
           (not (Not? (Assume-simulation-condition (first t)))))
          ;; (displayln "Assume - NOT NOT")
          (set-Assume-simulation-seen-before! (first t) #t)
          (map
           (append-item (first t))
           (expand-error-trace-helper (rest t) (append (Single-branch-branch (first instr-list)) (rest instr-list))
                                      interceptors-seen))]

         ;; If this was not a condition that appeared in the original interleaving
         ;; - we must explore both branches
         [else
          ;; (displayln "Other assume")
          (append
           (map 
            (append-item (Assume-simulation (Single-branch-condition (first instr-list)) #f))
            (expand-error-trace-helper t (append (Single-branch-branch (first instr-list))) interceptors-seen))

           (map
            (append-item (Assume-simulation (Not (Single-branch-condition (first instr-list))) #f))
            (expand-error-trace-helper t (rest instr-list) interceptors-seen)))])]

      ;; TODO - Branch implementation
      [(Branch? (first instr-list))
       (displayln "TODO: Branch implementation missing")]


      ;; Follow Goto statement in the obvious way.
      ;; TODO: find-goto-starting-point-trace ensures that we do
      ;; recheck the same interference as last time
      [(Goto? (first instr-list))
       ;; (displayln "Reached goto case")


       (let
           ([original-m-instr-list (Method-instr-list m)])
         ;; (display "original trace: ") (displayln original-t)
         ;; (display "Trace goto call: ") (displayln           (find-goto-starting-point-trace original-t (Goto-goto-addr (first instr-list)) interceptors-seen))

         ;; (display "normal call: ") (displayln           (find-goto-starting-point original-m-instr-list (Goto-goto-addr (first instr-list))))


         (expand-error-trace-helper
          (find-goto-starting-point-trace original-t (Goto-goto-addr (first instr-list)) interceptors-seen)
          (find-goto-starting-point original-m-instr-list (Goto-goto-addr (first instr-list)))
          interceptors-seen))]

      [(Label? (first instr-list))
       (expand-error-trace-helper
        t (rest instr-list) interceptors-seen)]

      [(Label? (first t))
       (expand-error-trace-helper
        (rest t) instr-list interceptors-seen)]


      ;; If this is an incerceptor method - then it obviously won't
      ;; be in the original method instructions
      [(not (null? (C-Instruction-thread-id (first t))))
       ;; (displayln "Found interceptor!")
       ;; (displayln (first t))
       ;; (displayln (rest t))
       ;; (displayln instr-list)
       (map
        (append-item (first t))
        (expand-error-trace-helper (rest t) instr-list (append (list (first t)) interceptors-seen)))]
        
      
      [(equal? (C-Instruction-instr-id (first t)) (C-Instruction-instr-id (first instr-list)))
       ;; (displayln "Found matching instructions!")

       (map
        (append-item (first t))
        (expand-error-trace-helper (rest t) (rest instr-list) interceptors-seen))]

      [else
       ;; (displayln "Reached else case")
       ;; (displayln (first t))
       ;; (displayln (rest t))
       ;; (displayln instr-list)
       (map

        
        (append-item (first instr-list))

        (if (not (null? (C-Instruction-thread-id (first t))))
            (expand-error-trace-helper t (rest instr-list) (append (list (first t)) interceptors-seen))
            (expand-error-trace-helper t (rest instr-list) interceptors-seen)))]))
            
                     


  
  (let
      ([m-instr-list (Method-instr-list m)])

    ;; (displayln "Running helper for the first time")
    (expand-error-trace-helper original-t m-instr-list `())))

















;; This takes an instruction list and finds the point addressed by Goto
;; TODO: This assumes that composed method has a Label "start" 
(define (find-goto-starting-point instr-list goto-addr)
  (cond
    [(empty? instr-list) `()]
    [(and (Label? (first instr-list)) (equal? (Label-id (first instr-list)) goto-addr))
     instr-list]
    [else
     (find-goto-starting-point (rest instr-list) goto-addr)]))

;; This takes a trace and finds the point addressed by Goto
;; TODO: Right now this will just go in infinite circles.
;; what is still yet to do is that we need to exclude the same interruption
;; when we go back around
(define (find-goto-starting-point-trace instr-list goto-addr seen-before)

  (define (contains-instruction l i)
    (cond
      [(empty? l) #f]
      [(not (C-Instruction-instr-id i)) #f]
      [(null? (C-Instruction-instr-id i)) #f]
      [(not (C-Instruction-instr-id (first l))) (contains-instruction (rest l) i)]
      [(equal? (C-Instruction-instr-id (first l)) (C-Instruction-instr-id i))
       #t]
      [else
       (contains-instruction (rest l) i)]))
  
  (define (remove-seen-before instr-list seen-before)
    (cond
      [(empty? instr-list) `()]
      [(not (C-Instruction? (first instr-list)))
       (append (list (first instr-list))
               (remove-seen-before (rest instr-list) seen-before))]
      [(not (C-Instruction-instr-id (first instr-list)))
       (append (list instr-list)
               (remove-seen-before (rest instr-list) seen-before))]
      [(contains-instruction seen-before (first instr-list))
       ;; (displayln "") (display "Found seen before: ") (displayln (first instr-list))
       (remove-seen-before (rest instr-list) seen-before)]
      [else
       (append
        (list (first instr-list))
        (remove-seen-before (rest instr-list) seen-before))]))

    

    

  
  ;; (cond
  ;;   [(empty? instr-list) `()]
  ;;   ;; [(and (Label? (first instr-list)) (equal? (Label-id (first instr-list)) goto-addr))
  ;;   ;;  (remove-seen-before instr-list seen-before)]
  ;;   [else
  (let ([trace-from-goto (find-goto-starting-point instr-list goto-addr)])
    (remove-seen-before instr-list seen-before)))

  ;; ]))

  


;; Takes a trace and returns #f if the trace chooses meta-vars inconsistently
;; and is obviously infeasible, true if the trace may be feasible
(define (meta-vars-consistent? t)
  (define (check-for-contradictions t-prime meta-var truth-val)
    (cond
      [(empty? t-prime) #t]
      [(Assume-meta? (first t-prime))
       (and
        (or (not (equal? (Assume-meta-condition (first t-prime)) meta-var))
           truth-val)
        (check-for-contradictions (rest t-prime) meta-var truth-val))]
      [(Assume-not-meta? (first t-prime))
       (and
        (or (not (equal? (Assume-not-meta-condition (first t-prime)) meta-var))
            (not truth-val))
        (check-for-contradictions (rest t-prime) meta-var truth-val))]
      [else
       (check-for-contradictions (rest t-prime) meta-var truth-val)]))
                     
  
  (cond
    [(empty? t) #t]
    [(Assume-meta? (first t))
     (and
      (check-for-contradictions (rest t)
                                (Assume-meta-condition (first t))
                                #t)
      (meta-vars-consistent? (rest t)))]
    [(Assume-not-meta? (first t))
     (and
      (check-for-contradictions (rest t)
                                (Assume-not-meta-condition (first t))
                                #f)
      (meta-vars-consistent? (rest t)))]
    [else
     (meta-vars-consistent? (rest t))]))











;; Returns whether we had to do a restart for a given hole,
;; or if we just blasted through
(define (optimistic-stopped-hole? t hole)
  (define (find-next-assume-sim t)
    (cond
      [(empty? t) t]
      [(Assume-simulation? (first t)) (first t)]
      [else
       (find-next-assume-sim (rest t))]))

  
  (cond
    [(empty? t) #f]
    [(equal? (C-Instruction-instr-id (first t)) (Hole-method1 hole))
     (let ([next-assume-sim (find-next-assume-sim t)])
       (and
        (Assume-simulation? next-assume-sim)
        (not (Not? (Assume-simulation-condition next-assume-sim)))))]

    [else
     (optimistic-stopped-hole? (rest t) hole)]))
     
     
     


;; Takes a trace and returns a list of all of the optimistic conditions
(define (collect-all-optimistic-expressions t-list)

  (unique
   (lambda (opt-cond1 opt-cond2)
     (equal? (Optimistic-Condition-meta-var opt-cond1) (Optimistic-Condition-meta-var opt-cond2)))
  
   (reduce
    append
    (map (lambda (t)
           (map (lambda (ti)
                  (Set-var-assignment ti))
                (filter (lambda (ti)
                          (and
                           (Set-var? ti)
                           
                           (Optimistic-Condition? (Set-var-assignment ti))))
                        t)))
         t-list))))
  







(define (hole-start-match hole-list instr-id)
  (filter
   (lambda (h) (equal? (Hole-method1 h) instr-id))
   hole-list))

(define (hole-end-match hole-list instr-id)
  (filter
   (lambda (h) (equal? (Hole-method2 h) instr-id))
   hole-list))


(define new-lock-count (void))
(set! new-lock-count 0)
(define (new-lock)
  (set! new-lock-count (+ new-lock-count 1))
  new-lock-count)

(define (new-locks match-starts)
  (map
   (lambda (h) (cons h (new-lock)))
   match-starts))


(define (create-acquire-statements start-locks)
  (map
   (lambda (pair) (Lock (cdr pair)))
   start-locks))


(define (create-release-statements match-ends acquired-list)
  (filter
   (lambda (a) (not (null? a)))
   (map
    (lambda (pair)

      (let ([matching-pairs
             (filter (lambda (h) (and
                                  (equal? (Hole-method1 (car h)) (Hole-method1 pair))
                                  (equal? (Hole-interruptor (car h)) (Hole-interruptor pair))
                                  (equal? (Hole-method2 (car h)) (Hole-method2 pair))))
                     acquired-list)])
        
        (cond
          [(empty? matching-pairs) null]
          [else
           (let ([lock-to-release
                  (cdr
                   
                   (first matching-pairs))])

             (Unlock lock-to-release))])))
    match-ends)))
       
       

(define (start-in-branch branch-instr-list hole-list)
  (filter
   (lambda (h)
     (< 0
        (length (filter (lambda (instr) (equal? (Hole-method1 h) (C-Instruction-instr-id instr))) branch-instr-list))))
   hole-list))

(define (end-in-branch branch-instr-list hole-list)
  ;; (displayln "end in branch")
  (filter
   (lambda (h)
     (< 0
        (length (filter (lambda (instr) (equal? (Hole-method2 h) (C-Instruction-instr-id instr))) branch-instr-list))))
   hole-list))


(define (hole-contains h-list h)
  ;; (display "hole-contains: ") (displayln h-list) (displayln h) (displayln "--------")
  (> 0 (length (filter (lambda (h-prime) (and
                                          (equal? (Hole-method1 h) (Hole-method1 h-prime))
                                          (equal? (Hole-interruptor h) (Hole-interruptor h-prime))
                                          (equal? (Hole-method2 h) (Hole-method2 h-prime))))
                       h-list))))
  

(define (all-in-branch branch-instr-list hole-list)
  ;; (displayln "all-in-branch")
  (let ([start-holes (start-in-branch branch-instr-list hole-list)]
        [end-holes (end-in-branch branch-instr-list hole-list)])
    ;; (display "all-in-branch-results: ") (display "1:") (displayln start-holes) (display "2:")
    ;; (displayln end-holes) (displayln "------")
    (filter (lambda (h) (hole-contains end-holes h)) start-holes)))



(define (only-start-in-branch branch-instr-list hole-list)
  (let
      ([start-holes (start-in-branch branch-instr-list hole-list)]
       [all-holes (all-in-branch branch-instr-list hole-list)])
    (filter (lambda (h) (not (hole-contains all-holes h))) start-holes)))


(define (only-end-in-branch branch-instr-list hole-list)
  ;; (display "only-in-branch: (") (display branch-instr-list) (display hole-list) (displayln ")")
  (let
      ([end-holes (end-in-branch branch-instr-list hole-list)]
       [all-holes (all-in-branch branch-instr-list hole-list)])

    (filter (lambda (h) (not (hole-contains all-holes h))) end-holes)))

;; Repairs given Method using Minimal Locking Algorithm
(define (minimal-lock instr-list hole-list acquired-list)

  
  
  (cond
    [(empty? instr-list) `()]
    [(Single-branch? (first instr-list))
     (let ([just-ends (only-end-in-branch (Single-branch-branch (first instr-list)) hole-list)])
       ;; (displayln "have just-ends")
       (let ([releases (create-release-statements
                        just-ends
                        acquired-list)])

         (append

          
          (list (Branch (Single-branch-condition (first instr-list))
                        (minimal-lock (Single-branch-branch (first instr-list)) hole-list acquired-list)
                        releases))


          (minimal-lock (rest instr-list) hole-list acquired-list))))]
    
    [(Branch? (first instr-list))
     ;; TODO
     `()]
    [(Loop? (first instr-list))
     ;; TODO
     (displayln "TODO: Cannot lock loops")]
    [(C-Instruction? (first instr-list))
     (let
         ([match-starts (hole-start-match hole-list (C-Instruction-instr-id (first instr-list)))]
          [match-ends (hole-end-match hole-list (C-Instruction-instr-id (first instr-list)))])
       (let
           ([start-locks (new-locks match-starts)])
         (let
             ([new-acquired-list (append acquired-list start-locks)]
              [new-acquires (create-acquire-statements start-locks)]
              [new-releases (create-release-statements match-ends acquired-list)])

           (append
            new-acquires
            (list (first instr-list))
            new-releases
            (minimal-lock (rest instr-list) hole-list new-acquired-list)))))]))
           
            
              
          
           

       
       
     
