#lang racket
(require "../program_representation/simulator-structures.rkt")

(provide
 metasketch-announcement-strategy

 )

(define optimistic-count (void))
(set! optimistic-count 0)
(define (new-optimistic-id)
  (set! optimistic-count (+ optimistic-count 1))
  optimistic-count)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;; Metasketch for Base Library ;;;;;;;;;;;;;;;;;;;;;;


(define (metasketch-library-add-announcement library method-name)

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

          (append
           (list method-match)
           rest-of-methods)))))

          
          
  




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;; Metasketch for Composed Method ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;; Optimistic Concurrency repair protocol ;;;;;;;;;;;;;;;;;
(define (metasketch-optimistic-strategy instr-list hole)
  (metasketch-repair instr-list optimistic-repair-strt optimistic-repair-end hole))

  
;; TODO: Have to actually interpret Optimistic-Condition
(define (generate-optimistic-check-expression interruptor output-var)
  (Set-var output-var (Optimistic-Condition (new-optimistic-id))))

  


(define (optimistic-repair-strt instr-list hole)
  (list
   (first instr-list)
   (Atomic-Start-Marker)
   (generate-optimistic-check-expression (Hole-interruptor hole) "OPTIMISTIC")
   (Single-branch (Get-var "OPTIMISTIC")
                  (list
                   (Goto "START")))))

(define (optimistic-repair-end instr-list hole)
  (append
   (list
   (first (instr-list))
   (Atomic-End-Marker))

   (rest instr-list)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;; Announcement repair protocol ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (metasketch-announcement-strategy instr-list hole)
  (metasketch-repair instr-list announcement-repair-strt announcement-repair-end hole))


;; TODO: Decide how to check for an announcement that's been made
(define (generate-announcement-check interruptor output-var)
  (Set-var output-var "ANNOUNCEMENT"))

;; Tries to defend a given instr-list from a particular hole happening using
;; announcements
;; TODO: This only modifies the composed method - we clearly also need to
;; modify the base library
(define (announcement-repair-strt instr-list hole)
  (list
   (first instr-list)
   (Atomic-Start-Marker)
   (generate-announcement-check (Hole-interruptor hole) "ANNOUNCE")
   (Single-branch (Get-var "ANNOUNCE")
                  (list
                   (Goto  "START")))
   ))

(define (announcement-repair-end instr-list hole)
  (append
  (list
   (first instr-list)
   (Atomic-End-Marker))
  (rest instr-list)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Metasketcher takes as input an instr-list representing a composed method
;; as well as two functions, repair-strt and repair-end. These are the two
;; functions that define the repair that will be done. Repair functions take
;; the remaining instr-list, as well as the hole
(define (metasketch-repair instr-list repair-strt repair-end hole)

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
          (metasketch-repair (Single-branch-branch (first instr-list)) repair-strt repair-end hole)))

        (if branch-ends-with-hole?
            (repair-end (rest instr-list) hole)
            (metasketch-repair (rest instr-list) repair-strt repair-end hole))))]
    
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
           (Branch-branch1 (first instr-list)) repair-strt repair-end hole)
          (metasketch-repair
           (Branch-branch2 (first instr-list)) repair-strt repair-end hole)))
        
        ;; We may or may not have to complete the repair after the branch
        (if branch-ends-with-hole?
            (repair-end (rest instr-list) hole)
            (metasketch-repair (rest instr-list) repair-strt repair-end hole))))]

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
            (metasketch-repair (Loop-instr-list (first instr-list)) repair-strt repair-end hole)))

           (if ends-with-hole?
               (repair-end (rest instr-list) hole)
               (metasketch-repair (rest instr-list) repair-strt repair-end hole)))]

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
               (metasketch-repair loop-body repair-strt repair-end hole)))
             
             (metasketch-repair (rest instr-list) repair-strt repair-end hole)))]))]
            

           
    
       
       
         
      
    [(equal? (C-Instruction-instr-id (first instr-list)) (Hole-method1 hole)) ;; We've found our hole
     ;; (displayln "Found hole")
     (append
      (repair-strt instr-list hole) ;; Add whatever needs to be added to the first instruction
      (repair-end (rest instr-list) hole))]
    [else
     ;; (display "didn't find hole, found: ") (displayln (C-Instruction-instr-id (first instr-list)))
     (append (list (first instr-list))
     (metasketch-repair (rest instr-list) repair-strt repair-end hole))
     ]))
