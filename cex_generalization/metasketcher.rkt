#lang racket
(require "../program_representation/simulator-structures.rkt")
;; (require "../cex_generalization/read-back-answer.rkt")
(require (only-in "../cex_generalization/read-back-answer.rkt"
                  Method-Call-Short?
                  Opt-Inequality-elem1
                  Opt-Inequality-elem2
                  Method-Call-Short-name
                  Method-Call-Short-args)
         (only-in "../utilities/utilities.rkt"
                  get-lib-method
                  replace-lib-method
                  append-item
                  append-list
                  reduce
                  unique)
         (only-in "to-sketch.rkt"
                  generate-optimistic-condition-sketches
                  generate-optimistic-condition-lists
                  generate-smart-optimistic-condition-grammar
                  generate-top-level-grammar
                  get-interfering-ret-vars
                  instr-list-to-sketch
                  generate-library-code)
         )


(require "../optimal_cover/cover.rkt" )
;; (require "to-sketch.rkt")


(provide
 metasketch-announcement-strategy
 metasketch-library-add-announcement
 optimistic-sketch-from-hole-list
 expand-traces-to-sketch-lib
 metasketch-optimistic-strategy
 optimistic-verification-condition
 modify-library-for-optimistic
 meta-vars-consistent?
 collect-all-optimistic-checks
 collect-all-optimistic-expressions
 optimistic-stopped-hole?
 optimistic-merge
 optimistic-grammar-sketch
 minimal-lock
 optimistic-repair
 connect-solutions-end-to-end
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



;; Determines if a Run-Method is a read or not. Works for lists and maps. 
(define (is-read? m)
  (cond
    [(and
      (Run-method? m)
      (or
       (equal? (Run-method-method m) "get")
       (equal? (Run-method-method m) "contains")))]
    [else
     #f]))

;; Optimistic Concurrency: Generates the grammar of possible optimistic conditions
(define (optimistic-grammar-sketch trace hole)
  ;; (display "grammar sketch generation: ") (displayln (first trace))
  (cond
    [(empty? trace) `()]
    [(and
      (C-Instruction? (first trace))
      (equal? (C-Instruction-instr-id (first trace)) (Hole-method1 hole))
      (Run-method? (first trace)))
     (list (first trace))]
             
    [(and
      (C-Instruction? (first trace))
      (equal? (C-Instruction-instr-id (first trace)) (Hole-method1 hole)))
     `()]

    [(and
      (Run-method? (first trace))
      (is-read? (first trace)))
     (append
      (list (first trace))
      (optimistic-grammar-sketch (rest trace) hole))]
    [else
     (optimistic-grammar-sketch (rest trace) hole)]))
     
      
             



      





;; Optimistic Concurrency: Merges traces that should really be a single trace with
;; a single optimistic condition.
;; Takes in:
;;           hole1, hole2: The holes that led to the generated trace
;;           trace1, trace2: the original traces
;;      returns:
;;           a list of traces. Either a single merged trace, or the two original traces
;;;;;;;;;;;;;;;;;;;;;;TODO ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (optimistic-merge opt-trace1 opt-trace2)
  (cond
    [(and
      (equal? (Hole-method1 (Optimistic-Trace-hole opt-trace1))
              (Hole-method1 (Optimistic-Trace-hole opt-trace2)))
      (equal? (Hole-method2 (Optimistic-Trace-hole opt-trace1))
              (Hole-method2 (Optimistic-Trace-hole opt-trace2))))
     (set-Optimistic-Trace-id! opt-trace2 (Optimistic-Trace-id opt-trace1))]
    [else
     (void)]))
              
  


(define (lock-merge lock-list)
  ;; Assumes locks are sorted


  
  (cond
    [(< (length lock-list) 2)
     lock-list]
    [(and
      (equal? (Hole-method1 (first lock-list)) (Hole-method1 (second lock-list)))
      (equal? (Hole-method2 (first lock-list)) (Hole-method2 (second lock-list))))
     (append
      (list (Hole (Hole-method1 (first lock-list)) (flatten (list (Hole-interruptor (first lock-list))
                                                         (Hole-interruptor (second lock-list))))
                  (Hole-method2 (first lock-list))))
      (lock-merge (rest (rest lock-list))))]
    [(and
      (equal? (Hole-method2 (first lock-list)) (Hole-method1 (second lock-list)))
      (equal? (Hole-interruptor (first lock-list)) (Hole-interruptor (second lock-list))))
     (append
      (list (Hole (Hole-method1 (first lock-list))  (Hole-interruptor (first lock-list))
                  (Hole-method2 (second lock-list))))
      (lock-merge (rest (rest lock-list))))]

    [(and
      (< (Hole-method1 (first lock-list)) (Hole-method1 (second lock-list)))
      (> (Hole-method2 (first lock-list)) (Hole-method2 (second lock-list)))
      (equal? (Hole-interruptor (first lock-list)) (Hole-interruptor (second lock-list))))

     (lock-merge 
      (append
       (list (first lock-list))
       (rest (rest lock-list))))]
    [(and
      (< (Hole-method1 (second lock-list)) (Hole-method1 (first lock-list)))
      (> (Hole-method2 (first lock-list)) (Hole-method2 (second lock-list)))
      (equal? (Hole-interruptor (first lock-list)) (Hole-interruptor (second lock-list))))
     (lock-merge 
      (append
       (list (second lock-list))
       (rest (rest lock-list))))]

    [else
     (append
      (list (first lock-list))
      (lock-merge (rest lock-list)))]))
      
  ;; (cond
     
  ;;   [(and
  ;;     (equal? (Hole-method1 (first h1)) (Hole-method1 h2))
  ;;     (equal? (Hole-method2 h1) (Hole-method2 h2)))
  ;;    (list (Hole (Hole-method1 h1) (list (Hole-interruptor h1) (Hole-interruptor h2)) (Hole-method2 h1)))]
  ;;   [(and
  ;;     (equal? (Hole-method2 h1) (Hole-method1 h2))
  ;;     (equal? (Hole-interruptor h1) (Hole-interruptor h2)))
  ;;    (list (Hole (Hole-method1 h1) (list (Hole-interruptor h1)) (Hole-method2 h2)))]
  ;;   [else
  ;;    (list h1 h2)]))



;; Optimistic Concurrency: Generate a set of traces to act as a verification condition
;; returns a set of pairs of traces, and whether the optimistic condition should
;; succeed or fail
;; Takes in:
;;          trace: Should have no interruptions
;;          hole: Should be a concrete hold with the offending interruptor as a runnable
;;                  statement
(define (optimistic-verification-condition trace hole opt-id)
  ;; (displayln trace)
  (cond
    [(empty? trace) (list (list))]

    [(Create-var? (first trace))
     (map
      (append-item (first trace))
      (optimistic-verification-condition (rest trace) hole opt-id))]
    [(and
      (C-Instruction? (first trace))
      (equal? (C-Instruction-instr-id (first trace)) (Hole-method1 hole)))

     ;; (display "found: ") (displayln (C-Instruction-instr-id (first trace)))
     ;; (display "found-hole: ") (displayln (Hole-method1 hole))
     ;; (display "to-end-of-hole: ") (displayln (to-end-of-hole (rest trace) hole))
     (map (append-list (list (first trace)
                             (Optimistic-Check opt-id #f)))
          (map
           (lambda (l) (append l (after-hole (rest trace) hole)))
           (shuffle-in (to-end-of-hole (rest trace) hole) (Hole-interruptor hole) opt-id)))]

          

    [else
     (map (append-list (list (first trace) ;; (Optimistic-Check opt-id  #f)
                             ))
          (optimistic-verification-condition (rest trace) hole opt-id))]))

(define (to-end-of-hole trace hole)
  (cond
    [(empty? trace) `()]
    [(and
      (C-Instruction? (first trace))
      (equal? (C-Instruction-instr-id (first trace)) (Hole-method2 hole)))
     `()]
    [else
     (append (list (first trace)) (to-end-of-hole (rest trace) hole))]))

(define (after-hole trace hole)
  (cond
    [(empty? trace) `()]
    [(and
      (C-Instruction? (first trace))
      (equal? (C-Instruction-instr-id (first trace)) (Hole-method2 hole)))
     trace]
    [else
     (after-hole (rest trace) hole)]))


(define (shuffle-in trace instr opt-id)
  ;; (display "shuffling: ") (displayln trace)
  (cond
    [(empty? trace) (list (list
                           instr
                           (Optimistic-Check opt-id #t)))]
    [(null? instr)
     (list trace)]

    ;; This way we don't have to remove the interfering things beforehand
    [(boolean? (C-Instruction-thread-id (first trace)))
     (shuffle-in (rest trace) instr opt-id)]
    
    [else
     (append
      (map (lambda (l) (append (list (first trace)
                                     instr
                                     (Optimistic-Check opt-id #t))
                               l))
                                    
           (shuffle-in (rest trace) `() opt-id))
      
      (map (lambda (l) (append (list (first trace)
                               l)))
                                    
           (shuffle-in (rest trace) instr opt-id)))]))
      

      

      
  




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
  



(define (collect-all-optimistic-checks t-list)
  (unique (lambda (opt-check1 opt-check2)
            (equal? (Optimistic-Check-opt-id opt-check1) (Optimistic-Check-opt-id opt-check2)))

          (reduce
           append
           (map
            (lambda (t)
              (filter (lambda (ti) ;; (displayln ti)
                              (Optimistic-Check? ti)) t))
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




;; (define (from-before-hole instr-list h)
;;   (cond
;;     [(empty? 

(define (remove-all-locks instr-list h locknum)
  (cond
    [(empty? instr-list)
     `()]
    [(Single-branch? (first instr-list))
     (append
      (list (Single-branch (Single-branch-condition (first instr-list))
                           (remove-all-locks (Single-branch-branch (first instr-list))
                                             h
                                             locknum)))
      (remove-all-locks (rest instr-list) h locknum))]
    [(Branch? (first instr-list))
     (append
      (list
       (Branch (Branch-condition (first instr-list))
               (remove-all-locks (Branch-branch1 (first instr-list)) h locknum)
               (remove-all-locks (Branch-branch2 (first instr-list)) h locknum)))
      (remove-all-locks (rest instr-list) h locknum))]
    [(and
      (Lock? (first instr-list))
      (equal? (Lock-id (first instr-list)) locknum))
     (remove-all-locks (rest instr-list) h locknum)]
    [else
     (append
      (list (first instr-list))
      (remove-all-locks (rest instr-list) h locknum))]))




(define (from-before-hole instr-list h locknum)
  ;; (displayln "from before hole")
  ;; (displayln instr-list)
  ;; (displayln h)
  ;; (displayln "------------------")
  (cond
    [(empty? instr-list)
     `()]
    [(Single-branch? (first instr-list))
     (append
      (list (Single-branch (Single-branch-condition (first instr-list))
                           (from-before-hole (Single-branch-branch (first instr-list))
                                             h
                                             locknum)))
      (from-before-hole (rest instr-list) h locknum))]
    [(Branch? (first instr-list))
     (append
      (list
       (Branch (Branch-condition (first instr-list))
               (from-before-hole (Branch-branch1 (first instr-list)) h locknum)
               (from-before-hole (Branch-branch2 (first instr-list)) h locknum)))
      (from-before-hole (rest instr-list) h locknum))]
    [(and
      (C-Instruction? (first instr-list))
      (equal? (C-Instruction-instr-id (first instr-list))
              (hole-before h)))

     ;; (display "LOCKING: ") (displayln locknum) (displayln "")

     (append
      (list (Lock locknum))
      (list (first instr-list))
      (remove-all-locks (rest instr-list) h locknum))]
    [else
      (from-before-hole (rest instr-list) h locknum)]))


(define (hole-ends-inside s h)
  (cond
    [(empty? s) #f]
    [(and
      (C-Instruction? (first s))
      (equal? (C-Instruction-instr-id (first s))
              (hole-after h)))
     #t]
    [else
     (or
      #f
      (hole-ends-inside (rest s) h))]))
     

            
(define (connect-solutions-end-to-end s1 s2 h1 h2  locknum switch)

  
  (cond
    [(empty? s1) `()]

    [(Single-branch? (first s1))

       
     (append
      (list (Single-branch (Single-branch-condition (first s1))
                           (connect-solutions-end-to-end
                            (Single-branch-branch (first s1))
                            s2
                            h1
                            h2
                            locknum
                            switch)))
      (connect-solutions-end-to-end
       (rest s1)
       s2
       h1
       h2
       locknum
       switch))]
 
      ;; (if (hole-ends-inside (Single-branch-branch (first s1)) h1)
      ;;     (remove-all-locks (rest s1) h2 locknum)
      ;;     (connect-solutions-end-to-end
      ;;                       (rest s1)
      ;;                       s2
      ;;                       h1
      ;;                       h2
      ;;                       locknum
      ;;                       switch)))]
          



            
    [(and (C-Instruction? (first s1))
          (equal?
           (C-Instruction-instr-id (first s1))
           (hole-after h1)))
     
     (append
      (list (Lock locknum) (first s1) )
      (connect-solutions-end-to-end
       (rest s1)
       s2
       h1
       h2
       locknum
       switch))]

      ;; (from-before-hole s2 h2 locknum))]
    [else
     (append
      (list (first s1))
      (connect-solutions-end-to-end (rest s1) s2 h1 h2 locknum switch))]))
         
          
          

          

                          

(define (combine-solutions s1 s2 h1 h2)
  (cond
    [(equal?
      (Hole-method2 h1)
      (Hole-method1 h2))
     (connect-solutions-end-to-end s1 s2 h1 h2 0)]))


(define (not-released match-ends new-acquired-list)

  (cond
    [(list? match-ends)
     (filter
      (lambda (i)
        (not (member i match-ends))
        )
      new-acquired-list)]
    [else
     (filter
      (lambda (i)
        (not (equal? match-ends i)))
      new-acquired-list)]))




(define (release-remaining acquired-list id)
  ;; (display "releasing what's left of ") (displayln acquired-list)
  ;; (display "current: ") (displayln id)
  (let*
      ([to-release
        (filter
         (lambda (l)
           (and
            (< id (Hole-method2 (car l)))
            (> id (Hole-method1 (car l)))))
         acquired-list)])
    (map (lambda (l) (Unlock (cdr l))) to-release)))


(define (hole-sort h-list)
  (sort h-list
        (lambda (a b)
          (< (Hole-method1 a) (Hole-method1 b)))))

;; Repairs given Method using Minimal Locking Algorithm
(define (minimal-lock instr-list hole-list-redundant acquired-list)

  ;; TODO: Solidify ordering of hole merges
  (define hole-list ;; (flatten (list (reduce lock-merge hole-list-redundant))))
    (lock-merge (hole-sort hole-list-redundant)))

  (display "merged locks: ") (displayln hole-list)
  (define returned-acquires (void))
  (set! returned-acquires `())

  ;; (display "Locking ") (displayln hole-list-redundant)
  ;; (display "found: ") (displayln instr-list)
  (cons
   (cond
     [(empty? instr-list) `()]
     [(Single-branch? (first instr-list))
      (let*
          ([just-ends (only-end-in-branch (Single-branch-branch (first instr-list)) hole-list)]
           [just-starts (only-start-in-branch (Single-branch-branch (first instr-list)) hole-list)])
        ;; (displayln "have just-ends")
        (let* ([releases (create-release-statements
                         just-ends
                         acquired-list)]
               [new-acquires (new-locks just-starts)])
          (let*
              ([branch-solution
                (minimal-lock (Single-branch-branch (first instr-list)) hole-list acquired-list)]
               [rest-solution (minimal-lock (rest instr-list) hole-list acquired-list)])
                
            (set! returned-acquires (append returned-acquires
                                            (cdr branch-solution) (cdr rest-solution)))
            (if (not (empty? releases))
                (append
                 
                 
                 (list (Branch (Single-branch-condition (first instr-list))
                               (car branch-solution )
                               releases))
               
               
                 (car rest-solution))
              
              (append
               
               
               (list (Single-branch (Single-branch-condition (first instr-list))
                                    (car (minimal-lock (Single-branch-branch (first instr-list)) hole-list acquired-list))))
               
               
               
               (car (minimal-lock (rest instr-list) hole-list (append acquired-list new-acquires))))))))]
     
     
     
     [(Return? (first instr-list))
      (let*
          ([rest-solution (minimal-lock (rest instr-list) hole-list acquired-list)])
        (set! returned-acquires (append returned-acquires (cdr rest-solution)))
         (append
          (release-remaining acquired-list (C-Instruction-instr-id (first instr-list)))
          (list (first instr-list))
          (car rest-solution)))]
      
     
     
     [(C-Instruction? (first instr-list))
      (let
          ([match-starts (hole-start-match hole-list (C-Instruction-instr-id (first instr-list)))]
           [match-ends (hole-end-match hole-list (C-Instruction-instr-id (first instr-list)))])
        (let
            ([start-locks (new-locks match-starts)])
          (let*
              ([new-acquired-list (append acquired-list start-locks)]
               [new-acquires (create-acquire-statements start-locks)]
               [new-releases (create-release-statements match-ends acquired-list)]
               [without-released (not-released match-ends new-acquired-list)]
               [rest-solution (minimal-lock (rest instr-list) hole-list new-acquired-list)])
            
            
            ;; (display "new acquired list: ")(displayln without-released)


            ;; We need to know which object and key to associate with each lock
            ;; this allows us to do local locking
            (set! returned-acquires
                  (unique
                   (lambda (a b) (equal? (cdr a) (cdr b)))
                   (append returned-acquires

                           (map
                            (lambda (p)
                              (cons
                               (Hole (Hole-method1 (car p))
                                     (first instr-list)
                                     (Hole-method2 (car p)))
                               
                               (cdr p)))
                            
                               

                            new-acquired-list)


                           (cdr rest-solution))))
            (append
             new-acquires
             (list (first instr-list))
             new-releases
             (car rest-solution)))))]
     
     )

   returned-acquires))

           
            

(define (generate-all-optimistic-expressions conditions)
  (define final-expr (void))
  (set! final-expr #t)
  (cons
   (reduce
    append
    (map
     (lambda (c)
       (let
           ([varname (string-append "TMP" (~v (freshvar)))])
         (cond
           [(Method-Call-Short? (Opt-Inequality-elem1 c))
            (if (boolean? final-expr)
                (set! final-expr (Not (Equal (Get-var varname)
                                                            (Get-var (Opt-Inequality-elem2 c)))))
                (set! final-expr (Or final-expr (Not (Equal (Get-var varname)
                                                            (Get-var (Opt-Inequality-elem2 c)))))))
                
            (list
             (Create-var varname "int")
             (Run-method
              (Method-Call-Short-name (Opt-Inequality-elem1 c))
              (Method-Call-Short-args (Opt-Inequality-elem1 c))
              varname))]
           [else
            (set! final-expr (And final-expr (Not (Equal varname (Opt-Inequality-elem1 c)))))
            (list
             (Create-var varname "int")
             (Run-method
              (Method-Call-Short-name (Opt-Inequality-elem2 c))
              (Method-Call-Short-args (Opt-Inequality-elem2 c))
              varname))])))
                   



    conditions))
   final-expr))
          
(define (repair-extension-instr-list instr-list hole id conditions)
  (cond
    [(empty? instr-list) `()]
    ;; [(and
    ;;   (Single-branch? (first instr-list))
    ;;   (not (empty? (Single-branch-branch (first instr-list))))
    ;;   (C-Instruction? (first (Single-branch-branch (first instr-list))))
    ;;   (equal?
    ;;    (C-Instruction-instr-id (first (Single-branch-branch (first instr-list))))
    ;;    (Hole-method1)))

    [(Single-branch? (first instr-list))
     (append
      (list (Single-branch
             (Single-branch-condition (first instr-list))
             (repair-extension-instr-list
              (Single-branch-branch (first instr-list))
              hole
              id
              conditions)))
      (repair-extension-instr-list (rest instr-list) hole id conditions))]
    [(and
      (C-Instruction? (first instr-list))
      (equal?
       (C-Instruction-instr-id (first instr-list))
       (Hole-method1 hole)))
     (append
      (list
       (Label id)
       (first instr-list))
      (repair-extension-instr-list (rest instr-list) hole id conditions))]

    [(and
      (C-Instruction? (first instr-list))
      (equal?
       (C-Instruction-instr-id (first instr-list))
       (Hole-method2 hole)))
     ;; (display "problem with: ") (displayln id)
     ;; (display "got: ") (hash-ref conditions id)
     (let*
         
         ([new-lock (+ id 100)] ;; TODO: make new lock
          [optimistic-expressions (generate-all-optimistic-expressions (hash-ref conditions id))])
       
       (append
        (list
         (Lock new-lock))
        (car optimistic-expressions)
        (list (Single-branch (cdr optimistic-expressions)
                             (list
                              (Unlock new-lock)
                              (Goto id 0)))
              
              (first instr-list)
              (Unlock new-lock))
        (repair-extension-instr-list (rest instr-list) hole id conditions)))]

         
     
    [else
     (append
      (list (first instr-list))
      (repair-extension-instr-list (rest instr-list) hole id conditions))]))
     
           

       
(define (optimistic-repair trace-list conditions library method-name [object "map"])
  (define extension-instr-list (void))
  (set! extension-instr-list
        (Method-instr-list (get-lib-method library method-name)))


  (display "opt-repair: ") (displayln extension-instr-list)
  (define already-seen (void))
  (set! already-seen `())
  (define (repair-extension t )
    (let*
        ([hole (Optimistic-Trace-hole t)]
         [id (Optimistic-Trace-id t)]
         [instr-list (Optimistic-Trace-instr-list t)])

      ;; (cond
      ;;   [(not (hash-has-key? conditions (string-append "OPT" (~v id))))
      ;;    (hash-set! conditions (string-append "OPT" (~v id)) ""

        
      (if (not (member id already-seen ))
          (begin
            (set! extension-instr-list
                  (repair-extension-instr-list extension-instr-list hole id conditions))

            (set! already-seen (append already-seen (list id))))
          (set! already-seen (append already-seen (list id))))))

  

  (for-each
   repair-extension
   trace-list)
  ;; (display "optimistic repair: ") (displayln extension-instr-list)
  extension-instr-list)
        
     




;; Code taken from optimistic example starts here

(define (next-method-call t)
  (cond
    [(empty? t) null]
    [(Run-method? (first t)) (first t)]
    [else
     (next-method-call (rest t))]))
     

;; TODO: This will not work once I implement argument selection in a more
;; reasonable way
(define (match-arguments a1 a2)
  (match a1
    [(Get-argument arg-num)
     (cond
       [(Get-argument? a2)
        (equal? a1 a2)]
       [(and
         (Get-var? a2)
         (equal? arg-num 0))
        (equal? (Get-var-id a2) "shared1")]
       [(and
         (Get-var? a2)
         (equal? arg-num 1))
        (equal? (Get-var-id a2) "shared2")])]

    [_
     (equal? a1 a2)]))

(define (find-matching-trace traces hole)


  
  ;; Assumes interruption is one line
  (define (matches-hole t )


    (cond
      [(empty? t) #f]
      [(and
        (equal? (hole-before hole) (C-Instruction-instr-id (first t)))
        (not (null? (next-method-call (rest t))))
        (equal? (hole-interrupt hole) (Run-method-method (next-method-call (rest t))))
        (match-arguments (first (Run-method-args (first t)))
                         (first (Run-method-args (next-method-call (rest t)))))
        ;; (equal? (first (Run-method-args (first t)))
        ;;         (first (Run-method-args (next-method-call (rest t)))))

        )
       (display "matching: ") (display (Run-method-args (first t)))
       (display "- ") (displayln (Run-method-args (next-method-call (rest t))))
       ;; ;; (display "found interruptor: ") ;; (displayln (Run-method-method (next-method-call (rest t))))
       #t]
      [else
       (matches-hole (rest t))]))
       
  
  (let ([matchings (filter matches-hole traces)])
    (cond
      [(empty? matchings) `()]
      [else
       (first matchings)])))



(define (convert-abstract-hole-to-concrete t hole)
  (cond
    [(empty? t) null]
    [(and
      (equal? (hole-before hole) (C-Instruction-instr-id (first t)))
      (not (null? (next-method-call (rest t))))
      (equal? (hole-interrupt hole) (Run-method-method (next-method-call (rest t)))))
     (Hole (hole-before hole) (next-method-call (rest t)) (hole-after hole))]
    [else
     (convert-abstract-hole-to-concrete (rest t) hole)]))
     
      

(define (arg-types-to-val arg-types [crucial-const "2"])
  (define count (void))
  (set! count 0)
  (reduce
   (lambda (a b) (string-append a " " b))
   (map
    (lambda (a)
      (match a
        ["int" crucial-const]
        [_ (set! count (+ count 1))
           (string-append "shared" (~v count))]))
    arg-types)))



(define (optimistic-sketch-from-hole-list result-trace-lists hole-set library [object "map"] 
                                          [arg-types `()] [crucial-const "2"])
  ;; ;; (displayln (second hole-set))

  ;; (display "hole-set: ") (displayln hole-set)
  


  ;; To repair a hole we need a witness to that hole
  (define witness-traces

    (map (lambda (h)
           (cons h (find-matching-trace result-trace-lists h)))
         hole-set))

  ;; (display "witnesses: ") (displayln witness-traces)

  ;; We then need to get a concrete instance of the hole - confusingly called a Hole


  (define all-concrete-holes
    (map
     (lambda (pair)
       (cons
        (convert-abstract-hole-to-concrete (cdr pair) (car pair))
        (cdr pair)))
     witness-traces))

  (display "concrete holes: ")
  (displayln all-concrete-holes)

  (cond
    [(empty? (flatten all-concrete-holes))
     (raise "no_solution")])



  (define all-declarations
    (unique
     (lambda (a b) (equal? (Create-var-id a) (Create-var-id b)))
     (reduce
      append
      (map
       (lambda (instr-list) (filter (lambda (instr)
                                      (Create-var? instr))
                                    instr-list))
       all-concrete-holes))))
  

  (display "declaractions: ") (displayln all-declarations)
    


  (define all-no-decl-witnesses
    (map
     (lambda (instr-list) (filter (lambda (instr)
                                    (or
                                     (Hole? instr)
                                     (and
                                      (C-Instruction? instr)
                                      (not (Create-var? instr)))))
                                  instr-list))
     all-concrete-holes))
  

  (display "all no decl witnesses: ") (displayln all-no-decl-witnesses)
  ;; (define all-no-interrupt-witnesses
  ;;   (map
  ;;    (lambda (instr-list) (filter (lambda (instr)
  ;;                                   (or
  ;;                                    (Hole? instr)
  ;;                                    (and
  ;;                                     (C-Instruction? instr)
  ;;                                     (not (boolean? (C-Instruction-thread-id instr))))))
  ;;                                 instr-list))
  ;;    all-no-decl-witnesses))

  ;; Why does this only sometimes happen?
  (define all-no-interrupt-witnesses all-no-decl-witnesses)


  
  
  (define counter-count (void))
  (set! counter-count 0)
  (define (counter-update)
    (set! counter-count (+ counter-count 1))
    counter-count)
  
  
  (define opt-trace-list
    (map
     (lambda (instr-list)
       (Optimistic-Trace
        (counter-update)
        (cdr instr-list)
        (car instr-list)))
     all-no-interrupt-witnesses))


  
  
  (optimistic-merge (first opt-trace-list) (second opt-trace-list))
  
  
  
  
  (define all-feasible-traces
    
    (reduce
     append
     (map
      (lambda (t) (optimistic-verification-condition (Optimistic-Trace-instr-list t) (Optimistic-Trace-hole t) (Optimistic-Trace-id t)))
      opt-trace-list)))

  (displayln "WITNESSES: ")
  (map (lambda (l) (displayln l)) all-feasible-traces)

  
  
  
  (displayln "got all feasible traces")
  (displayln all-feasible-traces)



  (define prelude (string-append "
    
#lang rosette
(require rosette/lib/synthax)

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
(struct List (first last) #:mutable)
(struct Shared_int (val) #:mutable)
(struct Stack (first) #:mutable)



(define shared1 (void))
(define shared2 (void))




(struct None ())
"
(cond
  [(equal? object "map")
   "(set! shared1 (List (None) (None)))
   (set! shared2 (List (None) (None)))"]
  [(equal? object "stack")
   "(set! shared1 (Stack (None))) (set! shared2 (Stack (None)))"])
"




(define shared (void))

(set! shared (Node  (None) \"test\" \"testval\" (None)))


(define first-args (void))
"
(cond
  [(equal? object "map")
   (string-append "(set! first-args (list " (arg-types-to-val arg-types crucial-const) "))" )] ;; (list shared1 shared2 1))"]
  [(equal? object "stack")
   "(set! first-args (list shared1 2))"])
"
(define POSSIBLE (void))
(set! POSSIBLE #t)

(define TRACE-TYPE (void))
(set! TRACE-TYPE \"no-optimistic-restart\")



(define RETURN-VAL (void))
(define TMP-RET (void))
(define-symbolic meta-var1 boolean?) ;; TODO collect meta-vars
(define OPTIMISTIC (void)) ;; TODO Need to do this automatically

(define-symbolic pick-trace integer?)"))




(define initialize-data-structure
  (cond
    [(equal? object "map")
     "(METHOD-push (list shared1 1 2))\n
(METHOD-push (list shared1 1 5))
\n(METHOD-push (list shared1 2 7))
\n(METHOD-push (list shared2 2 4))"]
[(equal? object "stack")
 "(METHOD-push (list shared1 5))"]))

     ;; ,(Run-method "push" `(,(Get-var "shared1") 1 2) null)
     ;; ,(Run-method "push" `(,(Get-var "shared1") 1 5) null)
     ;; ,(Run-method "push" `(,(Get-var "shared1") 2 7) null)
     ;; ,(Run-method "push" `(,(Get-var "shared2") 2 4) null))))









(define all-optimistic-checks (collect-all-optimistic-checks all-feasible-traces))
(displayln "optimistic checks")
(displayln all-optimistic-checks)


(define opt-cond-declarations (generate-optimistic-condition-sketches all-optimistic-checks 3))
(displayln "opt cond")
(displayln opt-cond-declarations)

(define opt-cond-list-defs (generate-optimistic-condition-lists all-optimistic-checks))
(displayln "opt-cond-list-defs")
(displayln opt-cond-list-defs)


(define opt-info-count (void))
(set! opt-info-count 0)
(define (new-opt-info-id)
  (set! opt-info-count (+ opt-info-count 1))
  opt-info-count)
(displayln "opt-info-count")
(displayln opt-info-count)


(define opt-info-list
  (map
   (lambda (h)
     (Optimistic-Info
      (new-opt-info-id)
      (optimistic-grammar-sketch (cdr (first all-concrete-holes)) (car (first all-concrete-holes)))))
   all-concrete-holes))

(displayln "opt-info-list")
(displayln opt-info-list)

(displayln (generate-smart-optimistic-condition-grammar opt-info-list "first-args" 2))

;; (define smart-opt-cond-list-defs
;;   (generate-smart-optimistic-condition-grammar opt-info-list arg-store depth





(define smart-opt-cond-top-level (generate-top-level-grammar opt-info-list "first-args" 1))
;; (displayln "top level")
;; (displayln smart-opt-cond-top-level)

;; (display "opt-info-list: ") ;; (displayln opt-info-list)

(define smart-opt-cond-decl (generate-smart-optimistic-condition-grammar opt-info-list "first-args" 1))
;; (displayln "cond level")
(displayln smart-opt-cond-decl)










(define trace-counter (void))
(set! trace-counter 0)



(displayln "Arrived at spit out traces definition")
(define (spit-out-traces traces hole lin-solutions)
  (define ret-string (void))
  (set! ret-string "")


  (displayln "interfering var decs")





  ;; (displayln


  ;;     (map (lambda (ti) (list (string-append "(define " (format "~a" ti) " (void))\n")))
  ;;          (get-interfering-ret-vars (second traces))))


  (displayln "printed interfering")




  
  (define interfering-var-declarations
    (reduce
     string-append
     (unique
      equal?
      (reduce
       append
       (map
        (lambda (which-trace)
          (reduce
           append
           (append
            (map (lambda (ti) (list (string-append "(define " (format "~a" ti) " (void))\n")))
                 (get-interfering-ret-vars which-trace))
            (list (list)))))
        traces)))))
  (displayln "interfering vars done")





  (set! ret-string (string-append ret-string  prelude))

  (set! ret-string (string-append ret-string  (reduce
   string-append
   (map (lambda (i) (string-append "(define POSSIBLE" (~v i) " (void))\n (set! POSSIBLE" (~v i) " #t)\n(define META-CHOICE" (~v i) " #t)"))
        (range (length traces))))))


  (displayln "adding declarations")
  (set! ret-string (string-append ret-string  (instr-list-to-sketch all-declarations library "first-args" 0 0) ))
  (displayln "declarations added")
  (set! ret-string (string-append ret-string  interfering-var-declarations))

  (set! ret-string (string-append ret-string   (generate-library-code library)))
  (set! ret-string (string-append ret-string  initialize-data-structure))

  ;; (set! ret-string (string-append ret-string  opt-cond-declarations)
  (set! ret-string (string-append ret-string  smart-opt-cond-decl))
  (set! ret-string (string-append ret-string  smart-opt-cond-top-level))
  (set! ret-string (string-append ret-string  opt-cond-list-defs))


  (set! ret-string (string-append ret-string  "(cond "))


  (displayln "going through traces")
  (map (lambda (which-trace)
         (set! trace-counter (+ trace-counter 1))
  
         ;; (map (lambda (l) (set! ret-string (string-append ret-string  l)) which-trace)

         
         (set! ret-string (string-append ret-string  (string-append "[(equal? pick-trace " (~v trace-counter) ")")))

         ;; (set! ret-string (string-append ret-string  "(set! ret-string (string-append ret-string  \"Trace: \")(set! ret-string (string-append ret-string  pick-trace)"))





           





         (set! ret-string (string-append ret-string 
          (string-replace
           (instr-list-to-sketch which-trace library "first-args" 0 0) 
          ;; (string-replace (instr-list-to-sketch which-trace optimistic-lib "first-args" 0  0) "POSSIBLE" (string-append "POSSIBLE" (~v trace-counter)))
          "META-CHOICE" (string-append "META-CHOICE" (~v trace-counter)))))
          

         ;; (set! ret-string (string-append ret-string  "(set! ret-string (string-append ret-string  \"POSSIBLE: \") (set! ret-string (string-append ret-string  POSSIBLE)\n")


         (set! ret-string (string-append ret-string  "
;; (set! ret-string (string-append ret-string  \"possible: \") (set! ret-string (string-append ret-string  POSSIBLE)
;; (set! ret-string (string-append ret-string  \"TRACE-TYPE: \") (set! ret-string (string-append ret-string  TRACE-TYPE)

]"))

       )
       traces)


  (set! ret-string (string-append ret-string  ")"))


  ;; (set! ret-string (string-append ret-string  (string-append "(assert (and (> pick-trace 0) (< pick-trace " (~v (+ 1 trace-counter)) ")))"))


;; (set! ret-string (string-append ret-string  (string-append "
;; (define (things-to-assert)
;;   (assert (or
;;            (equal? TRACE-TYPE \"optimistic-restart\")
;;            (equal? TRACE-TYPE \"broke-out\")
;;            (equal? TRACE-TYPE \"no-optimistic-restart\")))
;;   (assert
;;    (or
;;     (or (not (equal? TRACE-TYPE \"optimistic-restart\")) POSSIBLE)
;;     (or (not (equal? TRACE-TYPE \"broke-out\")) POSSIBLE)))
;;   (assert (or (not (equal? TRACE-TYPE \"no-optimistic-restart\")) (not POSSIBLE))))"))



(set! ret-string (string-append ret-string  "
(define (to-assert)"))
(for-each (lambda (opt-id)
            (set! ret-string (string-append ret-string  (string-append "
            (for-each (lambda (x) (assert x)) OPT" (~v (Optimistic-Check-opt-id opt-id)) "-true-list)
            (for-each (lambda (x) (assert (not x))) OPT" (~v (Optimistic-Check-opt-id opt-id)) "-false-list)"))))
          all-optimistic-checks)
(set! ret-string (string-append ret-string  "
  )"))



  
  (set! ret-string (string-append ret-string  "(define ANSWER (synthesize #:forall (list pick-trace)
                          #:guarantee (to-assert)))"))

(set! ret-string (string-append ret-string "(if (sat? ANSWER) (print-forms ANSWER) (displayln \"unsat\"))"))

  

ret-string

)

(displayln "past spit out definition")



;; (displayln "spitting out traces")
(list
 (spit-out-traces ;; (list (second feasible-traces))
                  all-feasible-traces
                  ;; concrete-hole
                  `()
                  
                  (list
                   (list (cons "RETURN-VAL" 5) (cons "push5-1" (None)))
                   (list (cons "RETURN-VAL" (None)) (cons "push5-1" 5))))
 opt-trace-list)



)
