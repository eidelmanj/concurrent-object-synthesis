#lang racket

(require "../program_representation/simulator-structures.rkt")

(provide
 unique
 append-list
 append-item
 all-combinations
 retrieve-code
 list-multiply
 range
 get-lib-method
 replace-lib-method
 transform-to-traces
 all-but-last
 reduce)

(define (all-but-last l) (reverse (cdr (reverse l))))
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
     



         
(define (get-lib-method library method-name)
  (let ([method-matches (filter (lambda (m) (equal? (Method-id m) method-name)) library)])
    (cond
      [(empty? method-matches) null]
      [else (first method-matches)])))

(define (replace-lib-method library method-name new-method)
  (let ([non-matches (filter (lambda (m) (not (equal? (Method-id m) method-name))) library)])
    (append (list new-method) non-matches)))



(define (list-multiply instr-list num)
  (cond
    [(equal? num 0) `()]
    [else
     (append instr-list (list-multiply instr-list (- num 1)))]))


(define (retrieve-code library id)
  (let ([lib-matches (filter (lambda (m) (equal? (Method-id m) id)) library)])
    (Method-instr-list (first lib-matches))))


(define (range n)
  (if (= n 0)
      (list 0)
      (append (list n) (range (- n 1)))))


(define (reduce func list)
  (if (null? (cdr list))
      (car list)
      (func (car list) (reduce func (cdr list)))))

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

(define (find-instr instr-list instr)
  (let 
      ([all-matches (filter (lambda (i)
                              (and (C-Instruction? i)
                                   (equal? (C-Instruction-instr-id i) (C-Instruction-instr-id instr))))
                            instr-list)])
    (cond
      [(empty? all-matches) null]
      [else
       (first all-matches)])))
   


  
  
