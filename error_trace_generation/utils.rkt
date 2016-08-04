#lang racket/base

(require (only-in racket/function curry negate)
         (only-in racket/list split-at splitf-at))

(provide pick-at-most-n
         splitf-at-after-index
         filter-hash)

; A helper function for pick-at-most-n.
; prev is a list of lists of length i - 1 containing elements from L.
; The helper constructs lists of length i by adding each element of L to the front
;  of each list contained in prev.
; The lists of at most n elements are the lists of elements of length i for i from
; 0 to n, inclusive.
(define (pick-at-most-n-helper L n i prev accumulator)
  (cond
    ; Add each element of L to the front of each list in prev
    [(<= i n) (define i-sets (apply append
                                    (map (λ (e)
                                           (map (curry cons e) prev))
                                         L)))
              (pick-at-most-n-helper L n (add1 i) i-sets (append accumulator i-sets))]
    [else accumulator]))

; Return a list of all lists of length at most n containing elements from L.
; Note that these are NOT sublists of L: the same element can appear in a
;  result list multiple times.
(define (pick-at-most-n L n)
  ; Initial values for prev and accumulator are the list of all lists of length 0
  (pick-at-most-n-helper L n 1 '(()) '(())))

(module+ test
  (require rackunit
           (only-in racket/set set))
  
  (define-syntax-rule (check-equal?-no-order actual expected)
    (check-equal? (apply set actual) (apply set expected)))
  
  (check-equal?-no-order (pick-at-most-n '() 0) '(()))
  (check-equal?-no-order (pick-at-most-n '(1 2 3) 0) '(()))
  (check-equal?-no-order (pick-at-most-n '(1 2 3) 1) '(() (1) (2) (3)))
  (check-equal?-no-order (pick-at-most-n '(1) 3) '(() (1) (1 1) (1 1 1)))
  (check-equal?-no-order (pick-at-most-n '(1 2) 3)
                         '(()
                           (1) (2)
                           (1 1) (1 2) (2 1) (2 2)
                           (1 1 1) (1 1 2) (1 2 1) (1 2 2)
                           (2 1 1) (2 1 2) (2 2 1) (2 2 2))))

; Split list L at the first item satisfying p that occurs with an index not less than i.
(define (splitf-at-after-index L p i)
  (let*-values ([(before-i after-i) (split-at L i)]
                [(before-p after-p) (splitf-at after-i (negate p))])
    (values (append before-i before-p) after-p)))

(module+ test
  (define-syntax-rule (check-equal?/values actual expected ...)
    (check-equal? (call-with-values (λ () actual) list) (list expected ...)))
  
  (check-equal?/values (splitf-at-after-index '(1 3 5 7 2 4 6 8) even? 0)
                       '(1 3 5 7) '(2 4 6 8))
  (check-equal?/values (splitf-at-after-index '(1 3 5 7 2 4 6 8) even? 1)
                       '(1 3 5 7) '(2 4 6 8))
  (check-equal?/values (splitf-at-after-index '(1 3 5 7 2 4 6 8) even? 4)
                       '(1 3 5 7) '(2 4 6 8))
  (check-equal?/values (splitf-at-after-index '(1 3 5 7 2 4 6 8) even? 5)
                       '(1 3 5 7 2) '(4 6 8))
  (check-equal?/values (splitf-at-after-index '(1 3 5 7 2 4 6 8) even? 7)
                       '(1 3 5 7 2 4 6) '(8))
  (check-equal?/values (splitf-at-after-index '(1 3 5 7 2 4 6 8) even? 8)
                       '(1 3 5 7 2 4 6 8) '())
  (check-equal?/values (splitf-at-after-index '(1 3 5 7 2 4 6 8) odd? 0)
                       '() '(1 3 5 7 2 4 6 8))
  (check-equal?/values (splitf-at-after-index '(1 3 5 7 2 4 6 8) negative? 0)
                       '(1 3 5 7 2 4 6 8) '()))

(define (filter-hash pred hash)
  (for/list ([(key value) hash]
             #:when (pred value))
    key))

(module+ test
  (check-equal?-no-order
   (filter-hash odd? (make-hash '((a . 1) (b . 2) (c . 3) (d . 4))))
   '(a c))
  (check-equal?-no-order
   (filter-hash even? (make-hash '((a . 1) (b . 2) (c . 3) (d . 4))))
   '(b d))
  (check-equal?-no-order
   (filter-hash negative? (make-hash '((a . 1) (b . 2) (c . 3) (d . 4))))
   '())
  (check-equal?-no-order
   (filter-hash positive? (make-hash '((a . 1) (b . 2) (c . 3) (d . 4))))
   '(a b c d)))