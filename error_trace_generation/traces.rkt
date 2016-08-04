#lang racket/base

(require (only-in "../program_representation/simulator-structures.rkt"
                  Run-method)
         (only-in "linearizable.rkt"
                  lin-result
                  lin-result-result
                  lin-result-trace
                  lin-result-client)
         (only-in racket/function curry)
         (only-in racket/match match define/match))

(provide minimal-traces)

; gr8 variable names
; Extends comparison to treat two Run-method structs as equal if they're calling
;  the same method.
(define/match (my-equal? v1 v2)
  [((Run-method #t _ id1 _ _) (Run-method #t _ id2 _ _)) (equal? id1 id2)]
  [(_ _) (equal? v1 v2)])

; Returns #t if L1 is a subsequence of L2, where elements are compared using
;  my-equal?, and #f otherwise.
(define/match (subsequence? L1 L2)
  [('() _) #t] ; the empty list is a sublist of anything
  [(_ '()) #f] ; nothing else is a sublist of the empty list

  ; Recursive step.
  ; TODO: maybe think about using drop-common-prefix?
  [(`(,H . ,T) L) (define L-tail (memf (curry my-equal? H) L))
                  (and L-tail (subsequence? T L-tail))])

(define (exists? pred L)
  (ormap pred L))

(define (minimal-traces results)
  (define sorted-results (sort results < #:key (compose length lin-result-client)))
  
  (define (results-helper L accumulator)
    (match L
      ['() accumulator]
      [`(,H . ,T)
       (define trace (lin-result-trace H))
       (results-helper
        T
        (if (exists? (λ (result) (subsequence? (lin-result-trace result) trace)) accumulator)
            accumulator
            (cons H accumulator)))]))
  
  (results-helper sorted-results '()))
