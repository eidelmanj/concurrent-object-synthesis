#lang racket/base #| CSC324 Backtracking Library |#

(require (only-in racket/list first rest))

#| API to Implement Backtracking Code. |#

(provide -< fail)

#| API to Use Backtracking Code. |#

(provide next has-next?)

#| Implementation. |#

(require (only-in racket/control abort prompt)
         racket/block)

(define no (λ ()
             (choice-points (list no))
             (abort)))

(define choice-points (make-parameter (list no)))

(define-syntax-rule (-< <expression> ...)
  (let/cc choice-point
    (choice-points
     (append (list (λ () (choice-point <expression>))
                   ...)
             (choice-points)))
    (fail)))

(define (pop-choice-point!)
  (define choice-point (first (choice-points)))
  (choice-points (rest (choice-points)))
  choice-point)

(define (fail)
  ((pop-choice-point!)))

(define (next)
  (prompt ((pop-choice-point!))))

; Gary didn't write this one.
(define (has-next?)
  (> (length (choice-points)) 1))