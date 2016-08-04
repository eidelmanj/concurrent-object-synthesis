#lang racket/base

(require (only-in "../program_representation/simulator-structures.rkt"
                  Run-method)
         (only-in "linearizable.rkt"
                  lin-result
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

(module+ test
  (require rackunit
           (only-in racket/function negate)
           (only-in "../program_representation/simulator-structures.rkt"
                    Run-Method))

  (define rm1 (Run-Method 'push '(1 2) 'ret #t))
  (define rm2 (Run-Method 'push '(1 2) 'ret #t))
  (define rm3 (Run-Method 'push '(2 1) 'ret #t))
  (define rm4 (Run-Method 'push '(1 2) 'ret2 #t))
  (define rm5 (Run-Method 'pull '(1 2) 'ret #t))
  (define rm6 (Run-Method 'push '(1 2) 'ret2 #f))

  (define (my-equal-rm1? rm) (my-equal? rm1 rm))
  (check-pred my-equal-rm1? rm2)
  (check-pred my-equal-rm1? rm3)
  (check-pred my-equal-rm1? rm4)
  (check-pred (negate my-equal-rm1?) rm5)
  (check-pred (negate my-equal-rm1?) rm6))

; Returns #t if L1 is a subsequence of L2, where elements are compared using
;  my-equal?, and #f otherwise.
(define/match (subsequence? L1 L2)
  [('() _) #t] ; the empty list is a sublist of anything
  [(_ '()) #f] ; nothing else is a sublist of the empty list

  ; Recursive step.
  ; TODO: maybe think about using drop-common-prefix?
  [(`(,H . ,T) L) (define L-tail (memf (curry my-equal? H) L))
                  (and L-tail (subsequence? T L-tail))])

(module+ test
  (check-true (subsequence? '() '()))
  (check-false (subsequence? '(1) '()))

  (define L1 `(,(Run-Method 'push '(1 2) 'ret #t)
               ,(Run-Method 'get '(1) 'ret2 #t)))
  (define (L1-subsequence? L2) (subsequence? L1 L2))
  (check-pred L1-subsequence? L1)
  (check-pred L1-subsequence? `(,(Run-Method 'push '(3 4) 'ret3 #t)
                                ,(Run-Method 'get '(2) 'ret4 #t)))

  (check-true (subsequence? '(2 3) '(1 2 3 4)))
  (check-true (subsequence? '(1) '(1 2 3 4)))
  (check-true (subsequence? '(3) '(1 2 3 4)))
  (check-true (subsequence? '(4) '(1 2 3 4))))

; Returns #t if some element of L satisfies pred, #f otherwise.
(define (exists? pred L)
  (ormap pred L))

(module+ test
  (check-false (exists? (λ (x) #t) '()))
  (check-true (exists? (λ (x) #t) '(#f)))
  (check-true (exists? odd? '(1 2 3 4 5)))
  (check-true (exists? odd? '(1 3 5 7 9)))
  (check-true (exists? odd? '(0 2 4 6 9)))
  (check-false (exists? negative? '(1 2 3 4 5))))

; Helper for minimal traces. Accumulates traces found to be minimal, assuming
;  L is sorted in non-decreasing order of client length.
(define (results-helper L accumulator)
  (match L
    ['() accumulator]
    [`(,H . ,T)
     (define trace (lin-result-trace H))
     (results-helper
      T
      (if (exists?
           (λ (result) (subsequence? (lin-result-trace result) trace))
           accumulator)
          accumulator
          (cons H accumulator)))]))

; Given a list of lin-result structs, return a list of only the minimal ones.
; A minimal result is one for which if any client instruction was removed, the
;  result would become linearizable.
(define (minimal-traces results)
  (define sorted-results (sort results < #:key (compose length lin-result-client)))
  (results-helper sorted-results '()))
