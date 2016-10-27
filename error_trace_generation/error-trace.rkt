#lang racket/base

(require (only-in "../program_representation/simulator-structures.rkt"
                  Method Method-args Method-instr-list
                  Run-method
                  C-Instruction
                  Single-branch Branch Loop
                  Create-var)
         "backtracking.rkt"
         "interpret.rkt"
         "linearizable.rkt"
         "methods.rkt"
         "traces.rkt"
         "vars.rkt"
         (only-in racket/match match)
         (only-in racket/list first split-at empty?)
         (only-in racket/function curryr))

(provide bound max-client-length error-traces test-init)

; The maximum number of non-commutative library calls to insert between each
;  pair of library calls in the mut when searching for linearizability errors.
(define bound (make-parameter 1))

; The maximum length of a client generated to attempt to find linearizability
;  errors. When set to #f, the mut will be fully instrumented during the search.
(define max-client-length (make-parameter #f))

(define (one-of L)
  (match L
    ['() (fail)]
    [`(,H . ,T) (-< H (one-of T))]))

; Stop values for the generator.
(define (stop-value) (values (void) (void) (void)))
(define (stop-value? x y z) (void? x))


;; We need a strict bound on the number of traces to try
(define stop-at (void))
(set! stop-at 100)

(define count-traces (void))
(set! count-traces 0)


(require racket/generator)
(define (instrumenter method library pointers)
  (define max-client (if (max-client-length)
                         (max-client-length)
                         (* (bound) (num-instrumentation-points method))))
  (generator ()
    (define (instrument mut breakpoint variables client)
      (cond
        [(>= (length client) max-client) (stop-value)]
        [else
         (define-values (before after) (split-at mut breakpoint))
         (match after
           ; Fully instrumented the method
           ['() (stop-value)]

           [`(,instr . ,tail)
            (match instr
              ; Library method call: add conflicting operations
              [(Run-method _ _ method args ret)
               (define ops (conflicting-ops (bound) instr library pointers))
               (define op (one-of ops)) ; choice point
               (define new-mut (append before (list instr) op tail))
               (define new-vars (append variables
                                        (map (curryr return-type library) op)))
               (define new-client (append client op))
               (unless (empty? op) (yield new-mut new-vars new-client))
               (instrument new-mut (+ breakpoint (length op) 1) new-vars new-client)]

              ; if without else: instrument the branch, then continue
              [(Single-branch _ _ condition branch)
               (define new-vars variables)
               (define new-client client)
               (for ([(trace vars ops)
                      (in-producer (instrumenter branch library pointers)
                                   stop-value?)])
                 (set! mut
                       (append before
                               `(,(Single-branch condition trace))
                               tail))
                 (set! new-vars (append variables vars))
                 (set! new-client (append client ops))
                 (yield mut new-vars new-client))
               (instrument mut (add1 breakpoint) new-vars new-client)]

              ; if with else: instrument a branch, then continue.
              ;  Then come back and instrument the other branch, then continue.
              [(Branch _ _ condition b1 b2)
               (define new-mut mut)
               (define new-vars variables)
               (define new-client client)

               ; We have to instrument both brances separately, otherwise we'll
               ;  end up with infeasible traces.
               (for ([(trace vars ops)
                      (in-producer (instrumenter b1 library pointers)
                                   stop-value?)])
                 (set! new-mut
                       (append before
                               `(,(Branch condition trace b2))
                               tail))
                 (set! new-vars (append variables vars))
                 (set! new-client (append client ops))
                 (yield new-mut new-vars new-client))
               (instrument new-mut (add1 breakpoint) new-vars new-client)

               ; Done with the first branch. Now try the second one.
               (set! new-mut mut)
               (set! new-vars variables)
               (set! new-client client)

               (for ([(trace vars ops)
                      (in-producer (instrumenter b2 library pointers)
                                   stop-value?)])
                 (set! new-mut
                       (append before
                               `(,(Branch condition b1 trace))
                               tail))
                 (set! new-vars (append variables vars))
                 (set! new-client (append client ops))
                 (yield new-mut new-vars new-client))
               (instrument new-mut (add1 breakpoint) new-vars new-client)]

              ; loop: instrument the loop body, then continue
              [(Loop _ _ condition body)
               (define new-vars variables)
               (define new-client client)
               (for ([(trace vars ops)
                      (in-producer
                       (instrumenter body library pointers) stop-value?)])
                 (set! mut
                       (append before `(,(Loop condition trace)) tail))
                 (set! new-vars (append variables vars))
                 (set! new-client (append client ops))
                 (yield mut new-vars new-client))
               (instrument mut (add1 breakpoint) new-vars new-client)]

              ; Anything else: continue iteration through the method
              [(C-Instruction _ _) (instrument mut (add1 breakpoint) variables client)])])]))

    (instrument method 0 '() '())))

(require (only-in racket/control prompt))

; Temporary, to be removed once init is no longer hard-coded.
(require (only-in "../program_representation/simulator-structures.rkt"
                  Set-var New-struct None Get-var Return))
(define test-init
  `(,(Create-var "shared" "Node")
    ,(Set-var "shared" (New-struct "Node" `(,(None) 0 0 0)))
    ,(Run-method "push" `(,(Get-var "shared") 1 2) null)
    ,(Run-method "push" `(,(Get-var "shared") 2 4) null)))

(define (error-traces library method-name init)
  (define mut (get-method method-name library))

  ; Construct a pointer table from init
  (define pointers (make-pointers-table init))

  ; Set up the interpreter
  (define lib (remove mut library))
  (define interpreter (make-interpreter lib))

  ; Set up the instrumenter
  (define numbered-mut (number-lines (Method-instr-list mut)))
  (define g (instrumenter numbered-mut lib pointers))

  ; Pick some arguments for the MUT
  (define arguments (pick-arguments mut lib pointers init))

  (define results '())


  ; why does this work?
  (prompt
   (for ([(trace vars client) (in-producer g stop-value?)])
     ; Three possible results: the trace is linearizable, the trace is not
     ;  linearizable, or we couldn't come up with arguments to make the trace
     ;  feasible.

     (define result
       (linearizable trace mut client vars pointers lib interpreter arguments init))
     (displayln trace)
     (unless (or (lin-result-t/f result) (> count-traces stop-at))
       (unless (null? (lin-result-trace result))
         (set! count-traces (+ count-traces 1))
         (display "counting: ")(displayln count-traces)
         (set! results (append results (list result))))
       (fail)))
   (when (and (> stop-at count-traces) (has-next?)) (next)))

  (values
   ; error traces
   (minimal-traces results)
   ; extension method with line numbers added
   (match mut
     [(Method id args ret-type _) (Method id args ret-type numbered-mut)])))










;; #lang racket/base

;; (require (only-in "../program_representation/simulator-structures.rkt"
;;                   Method Method-args Method-instr-list
;;                   Run-method
;;                   Run-method?
;;                   Get-var
;;                   Get-var-id
;;                   Get-var?
;;                   Run-method-method
;;                   Run-method-args
;;                   C-Instruction
;;                   Single-branch Branch Loop
;;                   Create-var)
;;          "backtracking.rkt"
;;          "interpret.rkt"
;;          "linearizable.rkt"
;;          "methods.rkt"
;;          "traces.rkt"
;;          "vars.rkt"
;;          (only-in racket/match match)
;;          (only-in racket/list take last)
;;          (only-in racket/list first split-at empty?)
;;          (only-in racket/function curryr))

;; (provide bound error-traces test-init)

;; ; The maximum number of non-commutative library calls to insert between each
;; ;  pair of library calls in the mut when searching for linearizability errors.
;; (define bound (make-parameter 1))

;; (define (one-of L)
;;   (match L
;;     ['() (fail)]
;;     [`(,H . ,T) (-< H (one-of T))]))

;; ; Stop values for the generator.
;; (define (stop-value) (values (void) (void) (void)))
;; (define (stop-value? x y z) (void? x))


;; ;; We need a strict bound on the number of traces to try
;; (define stop-at (void))
;; (set! stop-at 100000000000)

;; (define count-traces (void))
;; (set! count-traces 0)


;; (require racket/generator)
;; (define (instrumenter method library pointers)
;;   (generator ()
;;     (define (instrument mut breakpoint variables client)
;;       (define-values (before after) (split-at mut breakpoint))

;;       (match after
;;         ; Fully instrumented the method
;;         ['() (stop-value)]

;;         [`(,instr . ,tail)
;;          (match instr
;;            ; Library method call: add conflicting operations
;;            [(Run-method _ _ method args ret)

;;             (define (unsafe-args arg)
;;               (and
;;                (Get-var? arg)
;;                (or
;;                 (not (equal? (Get-var-id arg) "shared1"))
;;                 (not (equal? (Get-var-id arg) "shared2")))))
            
;;             ;; (define (unsafe-args arg-list)
;;             ;;   (not (empty?
;;             ;;         (filter
;;             ;;          not-shared
;;             ;;          arg-list))))
            


;;               ;; (1 >
;;               ;;    (length
;;               ;;     (filter
;;               ;;      (lambda (a)
;;               ;;        (and
;;               ;;         (Get-var? a)
;;               ;;         (and
;;               ;;                    (not (equal? (Get-var-id a) "shared1"))
;;               ;;                    (not (equal? (Get-var-id a) "shared2")))))
;;               ;;               arg-list))))

;;             (define (has-bad-args l)
;;               ;; (display "line: ") (displayln l)
;;               ;; (displayln (empty? (filter unsafe-args l)))
;;               (empty? (filter unsafe-args l)))

;;             (define (not-bad-method-call m)
;;               (cond
;;                 [(Run-method? m)
;;                  (not (has-bad-args (Run-method-args m)))]
;;                 [else
;;                  #t]))
;;             (define (remove-bad-method-calls m)
;;               (filter not-bad-method-call m))

;;             ;; (define all-ops
;;             ;;   (filter
;;             ;;    (lambda (l)
;;             ;;      (not (null? l)))
;;             ;;    (map
;;             ;;     remove-bad-method-calls
                
;;             ;;     (conflicting-ops (bound) instr library pointers))))

;;             (define all-ops (conflicting-ops (bound) instr library pointers))

            


            
;;             (define ops-push
;;               (filter
;;                (lambda (m)
;;                  (or (empty? m)


;;                      (equal? (Run-method-method (first m)) "push")))
;;                all-ops))

;;             (define ops-remove
;;               (filter
;;                (lambda (m)
;;                  (or (empty? m)


;;                       (equal? (Run-method-method (first m)) "remove")))



;;                all-ops))


            

;;             (define ops-push-remove
;;               (append ops-push ops-remove))


;;             (define ops ops-push-remove)
;;             ;; (define ops all-ops)


;;             ;; (displayln ops)
;;               ;; (list 
;;               ;;  (last

;;               ;; (filter
;;               ;;  (lambda (m)
;;               ;;    (or (empty? m) (equal? (Run-method-method (first m)) "remove")
;;               ;;        (equal? (Run-method-method (first m)) "push")))
                   
;;               ;;  (conflicting-ops (bound) instr library pointers)))
;;             ;; ))




;;               ;; (filter
;;                          ;;  (lambda (m)
;;                          ;;    (or
;;                          ;;     (list? m)
;;                          ;;     (equal? (Run-method-method m) "remove")))
                             
;;                           ;; (conflicting-ops (bound) instr library pointers)))
;;             (define op (one-of ops)) ; choice point
;;             (define new-mut (append before (list instr) op tail))
;;             (define new-vars (append variables
;;                                      (map (curryr return-type library) op)))
;;             (define new-client (append client op))
;;             (unless (empty? op) (yield new-mut new-vars new-client))
;;             (instrument mut (+ breakpoint 1) new-vars new-client)]



;;            ; if without else: instrument the branch, then continue
;;            [(Single-branch _ _ condition branch)
;;             (define new-vars variables)
;;             (define new-client client)
;;             (for ([(trace vars ops)
;;                    (in-producer (instrumenter branch library pointers)
;;                                 stop-value?)])
;;               (set! mut
;;                     (append before
;;                             `(,(Single-branch condition trace))
;;                             tail))
;;               (set! new-vars (append variables vars))
;;               (set! new-client (append client ops))
;;               (yield mut new-vars new-client))
;;             (instrument mut (add1 breakpoint) new-vars new-client)]

;;            ; if with else: instrument a branch, then continue.
;;            ;  Then come back and instrument the other branch, then continue.
;;            [(Branch _ _ condition b1 b2)
;;             (define new-mut mut)
;;             (define new-vars variables)
;;             (define new-client client)

;;             ; We have to instrument both brances separately, otherwise we'll
;;             ;  end up with infeasible traces.
;;             (for ([(trace vars ops)
;;                    (in-producer (instrumenter b1 library pointers)
;;                                 stop-value?)])
;;               (set! new-mut
;;                     (append before
;;                             `(,(Branch condition trace b2))
;;                             tail))
;;               (set! new-vars (append variables vars))
;;               (set! new-client (append client ops))
;;               (yield new-mut new-vars new-client))
;;             (instrument new-mut (add1 breakpoint) new-vars new-client)

;;             ; Done with the first branch. Now try the second one.
;;             (set! new-mut mut)
;;             (set! new-vars variables)
;;             (set! new-client client)

;;             (for ([(trace vars ops)
;;                    (in-producer (instrumenter b2 library pointers)
;;                                 stop-value?)])
;;               (set! new-mut
;;                     (append before
;;                             `(,(Branch condition b1 trace))
;;                             tail))
;;               (set! new-vars (append variables vars))
;;               (set! new-client (append client ops))
;;               (yield new-mut new-vars new-client))
;;             (instrument new-mut (add1 breakpoint) new-vars new-client)]

;;            ; loop: instrument the loop body, then continue
;;            [(Loop _ _ condition body)
;;             (define new-vars variables)
;;             (define new-client client)
;;             (for ([(trace vars ops)
;;                    (in-producer
;;                     (instrumenter body library pointers) stop-value?)])
;;               (set! mut
;;                     (append before `(,(Loop condition trace)) tail))
;;               (set! new-vars (append variables vars))
;;               (set! new-client (append client ops))
;;               (yield mut new-vars new-client))
;;             (instrument mut (add1 breakpoint) new-vars new-client)]

;;            ; Anything else: continue iteration through the method
;;            [(C-Instruction _ _) (instrument mut (add1 breakpoint) variables client)])]))

;;     (instrument method 0 '() '())))

;; (require (only-in racket/control prompt))

;; ; Temporary, to be removed once init is no longer hard-coded.
;; (require (only-in "../program_representation/simulator-structures.rkt"
;;                   Set-var New-struct None Get-var Return))
;; (define test-init
;;   `(,(Create-var "shared" "Node")
;;     ,(Set-var "shared" (New-struct "Node" `(,(None) 0 0 0)))
;;     ,(Run-method "push" `(,(Get-var "shared") 1 2) null)
;;     ,(Run-method "push" `(,(Get-var "shared") 2 4) null)))

;; (define (error-traces library method-name init)
;;   (define mut (get-method method-name library))

;;   ; Construct a pointer table from init
;;   (define pointers (make-pointers-table init))

;;   ; Set up the interpreter
;;   (define lib (remove mut library))
;;   (define interpreter (make-interpreter lib))

;;   ; Set up the instrumenter
;;   (define numbered-mut (number-lines (Method-instr-list mut)))
;;   (define g (instrumenter numbered-mut lib pointers))

;;   ; Pick some arguments for the MUT
;;   (define arguments (pick-arguments mut lib pointers init))

;;   (define results '())


;;   ; why does this work?
;;   (prompt
;;    (for ([(trace vars client) (in-producer g stop-value?)])
;;      ; Three possible results: the trace is linearizable, the trace is not
;;      ;  linearizable, or we couldn't come up with arguments to make the trace
;;      ;  feasible.
     
;;      ;; (set! count-traces (+ count-traces 1))
;;      ;; (display "counting: ")(displayln count-traces)

;;      (define result
;;        (linearizable trace mut client vars pointers lib interpreter arguments init))
;;      (displayln trace)
;;      (displayln "__________")
;;      (unless (or (lin-result-t/f result) (> count-traces stop-at))
;;        (unless (null? (lin-result-trace result))
;;          (set! results (append results (list result))))
;;        (fail)
;;        ))
;;    (when (and (> stop-at count-traces) (has-next?)) (next)))

;;   (values
;;    ; error traces
;;    (minimal-traces results)
;;    ; extension method with line numbers added
;;    (match mut
;;      [(Method id args ret-type _) (Method id args ret-type numbered-mut)])))
