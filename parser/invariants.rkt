#lang racket

(require "parser.rkt")
(provide all-args
         all-getters
         all-setters
         all-shared-accesses

         grab-program-node

         includes-vars
         find-equalities
         equivalent-vars
         annotate-if
         shared-vars)



;; Annotated if-node which contains the information of an if-node plus
;; lists of invariants imposed on each branch
(define-struct annotated-if-node (e p1 p2 inv1 inv2) #:transparent)

;; Collects all names in an arg-list struct into a list
;; arg-list -> string list
(define (all-args args)
  (match args
    [(arg-node (arg-decl v) next) (append (list v) (all-args next))]
    [(arg-add-node (arg-decl v) next) (append (list v) (all-args next))]
    [(empty-node) `()]))



;; Returns a list of all shared variable names
;; start-node -> string list  
(define (shared-vars strt)
  (let ([find-shared-list
         (lambda (node)
           (match node
             [(user-input-node s g p) (all-args s)]))])
    (match strt
      [(start-node u p) (find-shared-list u)])))


;; Returns all getter method names
;; start-node -> string list
(define (all-getters strt)
  (let ([find-getters
         (lambda (node)
           (match node
             [(user-input-node s g p) (all-args g)]))])
    (match strt
      [(start-node u p) (find-getters u)])))


;; Returns all setter method names
;; start-node -> string list
(define (all-setters strt)
  (let ([find-setters
         (lambda (node)
           (match node
             [(user-input-node s g p) (all-args p)]))])
    (match strt
      [(start-node u p) (find-setters u)])))


(define (grab-program-node strt)
  (match strt
    [(start-node u p) p]))






;; Given a list of shared variables and getter function names and a program node,
;; returns all GET accesses to those variables, along with line numbers

;; TODO: Line numbers are not currently accessible - For now just always 0
;; program-node -> string list -> string list -> access-record list
(define (all-shared-accesses shared-vars getter-list prgrm)
  (let ([grab-function-args
         (lambda (o)
                (match o
                  [(function-call-root (function-call-node v l)) (all-args l)]
                  [_ `()]))])
    
    (let ([grab-function-name
           (lambda (o)
             (match o
               [(single-var v) v]
               [(function-call-root (function-call-node v l)) v]
               [_ `()]))])
      
      (let ([grab-if-getter
             (lambda (s)
               (match s
                 [(assign-obj v1 v2 o cnt type)
                  (let ([fname (grab-function-name o)] [fargs (grab-function-args o)])
                    (if (and (member fname getter-list) (member v2 shared-vars))
                        (list (access-record v1 v2 fname fargs cnt type))
                        `()))]
                 [_ `()]))])
        
        (match prgrm
          [(program-node stmt next) (append (grab-if-getter stmt)
                                            (all-shared-accesses shared-vars getter-list next))]
          [(empty-node) `()]
          [_ (error 'typeException"Requires program-node")])))))






;; If any variables in var-list are in the expression e, return true. Otherwise false
;; string list -> expr -> bool
(define (includes-vars var-list e)
    (match e
      
      [(var-exp v) (if (member v var-list) #t #f)]
      [(arith-exp op s1 s2) (or (includes-vars var-list s1) (includes-vars var-list s2))]
      [_ #f]))



;; Given a program-node and a list of variables, returns all variables that should be related to
;; the variables in the list

;; string list -> prgrm -> (cons string expr)
(define (find-equalities var-list prgrm)
  (let ([add-vars
         (lambda (stmt)
           (match stmt
             [(expr-stmt (expr (assign-exp v e))) 
              (if (includes-vars var-list e) (list (cons v e)) (list (includes-vars var-list e)))]
             [_ (list)]))])
    (match prgrm
      [(program-node stmt next) (append (add-vars stmt) (find-equalities var-list next))]
      [_ `()])))
    

;; Given a list of GET assignments that have been made, returns lists of equivalent variables
;; all supposed to be the same as a particular shared access at a particular line number

;; TODO - Line numbers still not implemented - always 0
;; access-record list -> program-node -> (access-record (string list) pair)
(define (equivalent-vars access-list prgrm)
  (let ([find-vars
         (lambda (record)
           (match record
             [(access-record v1 v2 fname fargs line type)
              (cons record (find-equalities (list v1) prgrm))]))])
    
    (map find-vars access-list)))
                                                     

(define (generate-invariants shared-vars getters prgrm)
  (let ([assignment-record-list 
         (all-shared-accesses shared-vars getters prgrm)])
    ;; assignment-record-list))
    (equivalent-vars assignment-record-list prgrm)))



;; Returns an annotated if-else branch with the invariants generated by the statements inside it
;; Note: this function is not recursive.

(define (annotate-if node shared-vars getters setters)
             
  (match node
    [(if-node e p1 p2) (annotated-if-node e p1 p2
                                          (generate-invariants shared-vars getters p1)
                                          (generate-invariants shared-vars getters p2))]
    [_ (error 'typeException"node must be an if-node")]))

