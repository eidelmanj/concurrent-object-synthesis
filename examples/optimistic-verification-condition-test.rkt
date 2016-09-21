 #lang racket
(require "../program_representation/simulator-structures.rkt")
(require "../utilities/utilities.rkt")
(require "../examples/mooly-example.rkt")
(require "../examples/mooly-library.rkt")

(require racket/string)

(require "../cex_generalization/to-sketch.rkt")
(require "../cex_generalization/metasketcher.rkt")

(require "../error_trace_generation/error-trace.rkt")
(require (only-in "../error_trace_generation/linearizable.rkt" lin-result-trace))
(require (only-in "../error_trace_generation/methods.rkt" number-lines))
(require "../optimal_cover/cover.rkt" )



(define metasketch-lib ;; (metasketch-library-add-announcement library "extension"))
  library)
  ;; (modify-library-for-optimistic
  ;;  library
  ;;  "extension"
  ;;  (Hole 6 `() 7)))
  





(bound 1)
(define-values (err-traces numbered-method)
  (error-traces
   library
   "extension"

   `(,(Create-var "shared" "Node")
     ,(Create-var "primitive" "int")
     ,(Set-var "shared" (New-struct "Node" `(,(None) 0 0 0)))
     ,(Run-method "push" `(,(Get-var "shared") 1 2) null)
     ,(Run-method "push" `(,(Get-var "shared") 2 4) null))))


(define lib-with-nums (replace-lib-method library "extension" numbered-method))

(define results  err-traces)
(define result-trace-lists
  (map (lambda (t) (lin-result-trace t)) results))



(define hole-set (optimal-cover (map lin-result-trace results) null))




(define (next-method-call t)
  (cond
    [(empty? t) null]
    [(Run-method? (first t)) (first t)]
    [else
     (next-method-call (rest t))]))
     

(define (find-matching-trace traces hole)


  
  ;; Assumes interruption is one line
  (define (matches-hole t)
    (cond
      [(empty? t) #f]
      [(and
        (equal? (hole-before hole) (C-Instruction-instr-id (first t)))
        (not (null? (next-method-call (rest t))))
        (equal? (hole-interrupt hole) (Run-method-method (next-method-call (rest t)))))
       ;; (display "found interruptor: ") (displayln (Run-method-method (next-method-call (rest t))))
       #t]
      [else
       (matches-hole (rest t))]))
       
  
  (let ([matchings (filter matches-hole traces)])
    (cond
      [(empty? matchings) `()]
      [else
       (first matchings)])))



(define (convert-abstract-hole-to-concrete t hole)
  (cond
    [(empty? t) null]
    [(and
      (equal? (hole-before hole) (C-Instruction-instr-id (first t)))
      (not (null? (next-method-call (rest t))))
      (equal? (hole-interrupt hole) (Run-method-method (next-method-call (rest t)))))
     (Hole (hole-before hole) (next-method-call (rest t)) (hole-after hole))]
    [else
     (convert-abstract-hole-to-concrete (rest t) hole)]))
     
      


;; (display "Hole: ") (displayln  (second hole-set))

;; To repair a hole we need a witness to that hole
(define witness-traces

  (map (lambda (h)
       (cons h (find-matching-trace result-trace-lists h)))
       (second hole-set)))

;; (display "witnesses: ") (displayln (length witness-traces))
;; (displayln (first witness-traces))
;; (displayln (second witness-traces))


(define witness-trace (void))


;; We then need to get a concrete instance of the hole - confusingly called a Hole


(define all-concrete-holes
  (map
   (lambda (pair)
     (cons
      (convert-abstract-hole-to-concrete (cdr pair) (car pair))
      (cdr pair)))
   witness-traces))

;; (displayln all-concrete-holes)



(define all-declarations
  (unique
   (lambda (a b) (equal? (Create-var-id a) (Create-var-id b)))
   (reduce
    append
    (map
     (lambda (instr-list) (filter (lambda (instr)
                                    (Create-var? instr))
                                  instr-list))
     all-concrete-holes))))



    


(define all-no-decl-witnesses
  (map
   (lambda (instr-list) (filter (lambda (instr)
                                  (or
                                   (Hole? instr)
                                   (and
                                    (C-Instruction? instr)
                                    (not (Create-var? instr)))))
                                instr-list))
   all-concrete-holes))


(define all-no-interrupt-witnesses
  (map
   (lambda (instr-list) (filter (lambda (instr)
                                  (or
                                   (Hole? instr)
                                   (and
                                    (C-Instruction? instr)
                                    (not (boolean? (C-Instruction-thread-id instr))))))
                                 instr-list))
   all-no-decl-witnesses))


(define counter-count (void))
(set! counter-count 0)
(define (counter-update)
  (set! counter-count (+ counter-count 1))
  counter-count)


(define opt-trace-list
  (map
   (lambda (instr-list)
     (Optimistic-Trace
      (counter-update)
      (cdr instr-list)
      (car instr-list)))
   all-no-interrupt-witnesses))


      
(optimistic-merge (first opt-trace-list) (second opt-trace-list))




(define all-feasible-traces

  (reduce
   append
   (map
    (lambda (t) (optimistic-verification-condition (Optimistic-Trace-instr-list t) (Optimistic-Trace-hole t) (Optimistic-Trace-id t)))
    opt-trace-list)))





  (define prelude "
    
#lang rosette
(require rosette/lib/synthax)

(define current-thread (void))

(define method-exit (void))
(set! method-exit #f)


(define lock-list (void))
(set! lock-list (list))



(define (has-lock t-id lock)
  (cond
    [(> lock (- (length lock-list) 1))
     (set! lock-list (append lock-list (list t-id)))
     #t]
    [else
     (or (> 0 (list-ref lock-list lock)) (equal? t-id (list-ref lock-list lock)))]))



(define (replace-lock lock t-id)
  (cond
    [(equal? lock 0)
     (append (list t-id) (rest lock-list))]
    [else
     (append (list (first lock-list)) (replace-lock (- lock 1) t-id))]))

(define (get-lock t-id lock)
  (set! lock-list (replace-lock lock t-id)))

(define (release-lock t-id lock)
  (set! lock-list (replace-lock lock -1)))


(struct Node (next key val bits) #:mutable)

(define shared (void))
(struct None ())
(set! shared (Node  (None) \"test\" \"testval\" (None)))


(define first-args (void))
(set! first-args (list shared 1))
(define POSSIBLE (void))
(set! POSSIBLE #t)

(define TRACE-TYPE (void))
(set! TRACE-TYPE \"no-optimistic-restart\")



(define RETURN-VAL (void))
(define TMP-RET (void))
(define-symbolic meta-var1 boolean?) ;; TODO collect meta-vars
(define OPTIMISTIC (void)) ;; TODO Need to do this automatically

(define-symbolic pick-trace integer?)")


(define initialize-data-structure
  "(METHOD-push (list (list-ref first-args 0) (list-ref first-args 1) 5))")







(define all-optimistic-checks (collect-all-optimistic-checks all-feasible-traces))
;; (displayln all-optimistic-checks)


(define opt-cond-declarations (generate-optimistic-condition-sketches all-optimistic-checks 3))

;; (displayln opt-cond-declarations)

(define opt-cond-list-defs (generate-optimistic-condition-lists all-optimistic-checks))


(define opt-info-count (void))
(set! opt-info-count 0)
(define (new-opt-info-id)
  (set! opt-info-count (+ opt-info-count 1))
  opt-info-count)

(define opt-info-list
  (map
   (lambda (h)
     (Optimistic-Info
      (new-opt-info-id)
      (optimistic-grammar-sketch (cdr (first all-concrete-holes)) (car (first all-concrete-holes)))))
   all-concrete-holes))

;; (displayln (generate-smart-optimistic-condition-grammar opt-info-list "first-args" 2))

;; (define smart-opt-cond-list-defs
;;   (generate-smart-optimistic-condition-grammar opt-info-list arg-store depth





(define smart-opt-cond-top-level (generate-top-level-grammar opt-info-list "first-args" 2))

(define smart-opt-cond-decl (generate-smart-optimistic-condition-grammar opt-info-list "first-args" 2))










(define trace-counter (void))
(set! trace-counter 0)




(define (spit-out-traces traces hole lin-solutions)
  (define ret-string (void))
  (set! ret-string "")


  (define interfering-var-declarations
    (reduce
     string-append
     (unique
      equal?
      (reduce
       append
       (map
        (lambda (which-trace)
          (reduce
           append
           (map (lambda (ti) (list (string-append "(define " (format "~a" ti) " (void))\n")))
                (get-interfering-ret-vars which-trace))))
        traces)))))






  (set! ret-string (string-append ret-string  prelude))

  (set! ret-string (string-append ret-string  (reduce
   string-append
   (map (lambda (i) (string-append "(define POSSIBLE" (~v i) " (void))\n (set! POSSIBLE" (~v i) " #t)\n(define META-CHOICE" (~v i) " #t)"))
        (range (length traces))))))

  (set! ret-string (string-append ret-string  (instr-list-to-sketch all-declarations library "first-args" 0 0) ))
  (set! ret-string (string-append ret-string  interfering-var-declarations))
  (set! ret-string (string-append ret-string   (generate-library-code library)))
  (set! ret-string (string-append ret-string  initialize-data-structure))

  ;; (set! ret-string (string-append ret-string  opt-cond-declarations)
  (set! ret-string (string-append ret-string  smart-opt-cond-decl))
  (set! ret-string (string-append ret-string  smart-opt-cond-top-level))
  (set! ret-string (string-append ret-string  opt-cond-list-defs))


  (set! ret-string (string-append ret-string  "(cond "))


  (map (lambda (which-trace)
         (set! trace-counter (+ trace-counter 1))
  
         ;; (map (lambda (l) (set! ret-string (string-append ret-string  l)) which-trace)

         
         (set! ret-string (string-append ret-string  (string-append "[(equal? pick-trace " (~v trace-counter) ")")))

         ;; (set! ret-string (string-append ret-string  "(set! ret-string (string-append ret-string  \"Trace: \")(set! ret-string (string-append ret-string  pick-trace)"))





           





         (set! ret-string (string-append ret-string 
          (string-replace
           (instr-list-to-sketch which-trace library "first-args" 0 0) 
          ;; (string-replace (instr-list-to-sketch which-trace optimistic-lib "first-args" 0  0) "POSSIBLE" (string-append "POSSIBLE" (~v trace-counter)))
          "META-CHOICE" (string-append "META-CHOICE" (~v trace-counter)))))
          

         ;; (set! ret-string (string-append ret-string  "(set! ret-string (string-append ret-string  \"POSSIBLE: \") (set! ret-string (string-append ret-string  POSSIBLE)\n")


         (set! ret-string (string-append ret-string  "
;; (set! ret-string (string-append ret-string  \"possible: \") (set! ret-string (string-append ret-string  POSSIBLE)
;; (set! ret-string (string-append ret-string  \"TRACE-TYPE: \") (set! ret-string (string-append ret-string  TRACE-TYPE)

]"))

       )
       traces)


  (set! ret-string (string-append ret-string  ")"))


  ;; (set! ret-string (string-append ret-string  (string-append "(assert (and (> pick-trace 0) (< pick-trace " (~v (+ 1 trace-counter)) ")))"))


;; (set! ret-string (string-append ret-string  (string-append "
;; (define (things-to-assert)
;;   (assert (or
;;            (equal? TRACE-TYPE \"optimistic-restart\")
;;            (equal? TRACE-TYPE \"broke-out\")
;;            (equal? TRACE-TYPE \"no-optimistic-restart\")))
;;   (assert
;;    (or
;;     (or (not (equal? TRACE-TYPE \"optimistic-restart\")) POSSIBLE)
;;     (or (not (equal? TRACE-TYPE \"broke-out\")) POSSIBLE)))
;;   (assert (or (not (equal? TRACE-TYPE \"no-optimistic-restart\")) (not POSSIBLE))))"))



(set! ret-string (string-append ret-string  "
(define (to-assert)"))
(for-each (lambda (opt-id)
            (set! ret-string (string-append ret-string  (string-append "
            (for-each (lambda (x) (assert x)) OPT" (~v (Optimistic-Check-opt-id opt-id)) "-true-list)
            (for-each (lambda (x) (assert (not x))) OPT" (~v (Optimistic-Check-opt-id opt-id)) "-false-list)"))))
          all-optimistic-checks)
(set! ret-string (string-append ret-string  "
  )"))



  
  (set! ret-string (string-append ret-string  "(print-forms (synthesize #:forall (list pick-trace)
                          #:guarantee (to-assert)))"))

  

ret-string

)




(define spit-out (spit-out-traces ;; (list (second feasible-traces))
                  all-feasible-traces
                  ;; concrete-hole
                  `()
                  
                  (list
                   (list (cons "RETURN-VAL" 5) (cons "push5-1" (None)))
                   (list (cons "RETURN-VAL" (None)) (cons "push5-1" 5)))))

(with-output-to-file "TMP.rkt"
  (lambda () (printf spit-out)))

(system "/u/eidelmanj/racket/bin/racket TMP.rkt > OUTPUT.txt")
(system "rm -f TMP.rkt")


(displayln (file->string "OUTPUT.txt"))
