#lang racket

(provide
 unique
 append-list
 append-item
 all-combinations
 range
 reduce)


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

