#lang racket
(require "../program_representation/simulator-structures.rkt")

(provide
 add-optimistic-concurrency
 )


(define (create-choice-condition)
  #f)

(define optim-var-count (void))
(set! optim-var-count 0)
(define (new-optim-var)
  (set! optim-var-count (+ optim-var-count 1))
  optim-var-count)

(define (complete-optimistic-patch instr-list hole)
  (cond
   [(empty? instr-list) `()]
   [(Single-branch? (first instr-list))
    (append
     (list
      (Single-branch
       (complete-optimistic-patch (Single-branch-branch (first instr-list)))))
     (complete-optimistic-patch (rest instr-list)))]
   [(equal? (Get-instr-id (first instr-list)) (Hole-method2 hole))
     (list
      (Single-branch
        (create-choice-condition)
        (list
         (Continue)))
      (instr-list))]
       
                     
    
    
   [else
    (append (list (first instr-list)) (complete-optimistic-patch (rest instr-list) hole))]))
    



(define (patch-hole-optimistic-concurrency instr-list hole)
  (let ([new-var (new-optim-var)])
    (Meta-branch
     (list
      (Create-var new-var)
      (Loop
       (Get-var new-var)
       
       (complete-optimistic-patch instr-list hole)))
     instr-list)))
               
    
    
    

;; (define (confirm-hole instr-list hole)
;;   (cond
;;     [(empty? instr-list) #f]
;;     [(empty? (Hole-interruptor hole))
;;      (equal? (Hole-method2 hole) (Get-instr-id (first instr-list)))]
;;     [(equal? (Get-instr-id (first instr-list)) (first (Hole-interruptor hole)))
;;      (confirm-hole (rest instr-list) (Hole (Hole-method1 hole) (rest (Hole-interruptor hole)) (Hole-method2 hole)))]))

(define (confirm-hole instr-list hole)
  (cond
    [(empty? instr-list #f)]
    [else
     (equal? (Get-instr-id (first instr-list)) (Hole-method2 hole))]))

(define (get-relevant-hole instr-list hole-set)
  (cond
    [(empty? instr-list) (None)]
    [(empty? hole-set) (None)]

    [(Single-branch? (first instr-list))
     (let ([with-branch (append (Single-branch-branch (first instr-list)) (rest instr-list))])
       (if (not (None? (get-relevant-hole with-branch hole-set)))
           (get-relevant-hole with-branch hole-set)
           (get-relevant-hole (rest instr-list) hole-set)))]
     
    
    [(and (equal? (Get-instr-id (first instr-list)) (Hole-method1 (first hole-set)))
          (confirm-hole (rest instr-list) (first hole-set)))
     (first hole-set)]
          
     

    [else
     (get-relevant-hole instr-list (rest hole-set))]))
     

(define (add-optimistic-concurrency instr-list hole-set)
  (cond
    [(empty? instr-list) `()]
    [(not (None? (get-relevant-hole instr-list hole-set)))
     (let ([hole (get-relevant-hole instr-list hole-set)])
       (patch-hole-optimistic-concurrency instr-list hole))]
    [else
     (append (list (first instr-list)) (add-optimistic-concurrency (rest instr-list) hole-set))]))
