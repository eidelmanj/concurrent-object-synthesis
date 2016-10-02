#lang rosette
(require "../program_representation/simulator-structures.rkt")
(require "../utilities/utilities.rkt")
(require "../examples/mooly-example.rkt")
(require "../examples/mooly-library.rkt")

(require racket/string)

(require "../cex_generalization/to-sketch.rkt")
(require "../cex_generalization/read-back-answer.rkt")
(require "../cex_generalization/metasketcher.rkt")

(require "../error_trace_generation/error-trace.rkt")
(require (only-in "../error_trace_generation/linearizable.rkt" lin-result-trace))
(require (only-in "../error_trace_generation/methods.rkt" number-lines))
(require "../optimal_cover/cover.rkt" )


(provide
 (struct-out Method-Choice)
 interpret-method)


(define loop-count (void))
(set! loop-count 0)

(define (while condition f)
  (if (and (condition) (< loop-count 10))
      (begin
        (set! loop-count (+ loop-count 1))
        (f)
        (while condition f))
      (void)))


;; I feel like this must be defined somewhere else, but for now:
(define all-types (list "int" "char"))


(define-syntax-rule (append-to str new-str)
  (set! str (string-append str new-str)))

(struct Method-Choice (id meta-var choice-list))
(define choice-counter (void))
(set! choice-counter 0)
(define (new-choice-id)
  (set! choice-counter (+ choice-counter 1))
  choice-counter)

(define (generate-sequential-sketch-line library arg-types tmp-id)
  (define ret-str (void))
  (set! ret-str "")


  ;; (append-to ret-str (string-append "(define-symbolic meta" (~v choice-id) " integer?)"))
  (append-to ret-str (string-append "(Method-Choice " (~v tmp-id) " meta" (~v tmp-id) " (list \n"))


  ;; We choose a library method to call
  (for ([m library])
    (append-to ret-str (string-append
           "(Run-method \"" (Method-id m) "\" (list "))
    
    ;; Now we need to choose arguments
    (for ([tp (Method-args m)])
      
      (cond
        ;; If we don't need a pointer
        [(member tp all-types)
         (append-to ret-str   " (??) ")
         ]

        ;; If we do need a pointer
        [else
         (append-to  ret-str  " (Get-argument 0) ")
         ]))


    (append-to ret-str  (string-append ") \"tmp" (~v tmp-id) "\")\n")))

  (append-to ret-str "))\n")
  
  ret-str)




(define (generate-sequential-sketch library arg-types ret-type size)
  (define ret-str (void))
  (set! ret-str "")


  (append-to ret-str (string-append "(Method \"EXTSKETCH\" " (~v arg-types) " \""  ret-type "\" \n"))
  (append-to ret-str "(list \n")

  (for ([i (range size)])
    (append-to ret-str (string-append "(Create-var \"tmp" (~v i) "\" \"int\")\n"))
    )

  (for ([i (range size)])
    (append-to ret-str (generate-sequential-sketch-line library arg-types i))
    )


  (append-to ret-str "(Return (choose " )
  (for ([i (range size)])
    (append-to ret-str (string-append "(Get-var \"tmp" (~v i) "\") "))
    )
  (append-to ret-str "))))\n")



  ret-str
)
;; (displayln (generate-sequential-sketch library `() "int" 3))


(define (pretty-print-expr expr)
  (cond
    [(Dereference? expr)
     (string-append
      "(Dereference \"" (Dereference-id expr) "\" \""
      (Dereference-type expr) "\" \"" (Dereference-offset expr) "\")")]
    [(Equal? expr)
     (string-append
      "(Equal " (pretty-print-expr (Equal-expr1 expr)) " " (pretty-print-expr (Equal-expr2 expr)) ")")]
    [(Not-equal? expr)
     (string-append
      "(Not-equal " (pretty-print-expr (Not-equal-expr1 expr)) " " (pretty-print-expr (Not-equal-expr2 expr)) ")")]
    [(Not? expr)
     (string-append
      "(Not " (pretty-print-expr (Not-expr expr)) ")")]
    [(Or? expr)
     (string-append
      "(Or " (pretty-print-expr (Or-expr1 expr)) " " (pretty-print-expr (Or-expr2 expr)) ")")]
    [(And? expr)
     (string-append
      "(And " (pretty-print-expr (And-expr1 expr)) " " (pretty-print-expr (And-expr2 expr)) ")")]
    [(Get-var? expr)
     (string-append
      "(Get-var " (~v (Get-var-id expr)) ")")]
    [(Add? expr)
     (string-append
      "(Add " (pretty-print-expr (And-expr1 expr)) " " (pretty-print-expr (And-expr2 expr)) ")")]
    [(Subtract? expr)
     (string-append
      "(Subtract " (pretty-print-expr (Subtract-expr1 expr)) " " (pretty-print-expr (Subtract-expr2 expr)) ")")]
    [(Divide? expr)
     (string-append
      "(Divide " (pretty-print-expr (Divide-expr1 expr)) " " (pretty-print-expr (Divide-expr2 expr))")")]
    [(Multiply? expr)
     (string-append
      "(Multiply " (pretty-print-expr (Multiply-expr1 expr)) " " (pretty-print-expr (Multiply-expr2 expr)) ")")]
    [(Less-than? expr)
     (string-append
      "(Less-than " (pretty-print-expr (Less-than-expr1 expr)) " " (pretty-print-expr (Less-than-expr2 expr)) ")")]
    [(Less-than-equal? expr)
     (string-append
      "(Less-than-equal " (pretty-print-expr (Less-than-equal-expr1 expr)) " "
      (pretty-print-expr (Less-than-equal-expr2 expr)) ")")]
    [(Greater-than? expr)
     (string-append
      "(Greater-than " (pretty-print-expr (Greater-than-expr1 expr)) " "
      (pretty-print-expr (Greater-than-expr2 expr)) ")")]
    [(Greater-than-equal? expr)
     (string-append
      "(Greater-than-equal " (pretty-print-expr (Greater-than-equal-expr1 expr)) " "
      (pretty-print-expr (Greater-than-equal-expr2 expr)) ")")]
    [(Get-argument? expr)
     (string-append
      "(Get-argument " (~v (Get-argument-id expr)) ")")]
    [(Is-none?? expr)
     (string-append
      "(Is-none? " (pretty-print-expr (Is-none?-val expr)) ")")]
    [(New-struct? expr)
     (string-append
      "(New-struct \"" (New-struct-type expr) "\" " (~v (New-struct-arg-list expr)) ")")]
    [else
     (~v expr)]))
       

(define (pretty-print-c-instr instr)
  (cond
    [(not (C-Instruction? instr))
     "Warning: Not C instr\n"]
    [(Set-pointer? instr)
     ;; (display "set poitner: ") (displayln instr)
     (string-append "(Set-pointer " (~v (Set-pointer-id instr)) " " (~v (Set-pointer-type instr))
                    " " (~v (Set-pointer-offset instr)) " " (pretty-print-expr (Set-pointer-val instr)) ")\n")]
    [(CAS? instr)
     (string-append "(CAS " (CAS-v1 instr) " " (CAS-v2 instr) " " (CAS-new-val instr) " " (CAS-ret instr) ")\n")]
    [(Create-var? instr)
     (string-append "(Create-var \"" (Create-var-id instr) "\" \"" (Create-var-type instr) "\")\n")]
    [(Set-var? instr)
     (string-append
      "(Set-var " (~v (Set-var-id instr)) " " (pretty-print-expr (Set-var-assignment instr)) ")\n")]
    [(Lock? instr)
     (string-append
      "(Lock " (~v (Lock-id instr)) ")\n")]
    [(Unlock? instr)
     (string-append
      "(Unlock " (~v (Unlock-id instr)) ")\n")]
    [(Return? instr)
     (string-append
      "(Return " (pretty-print-expr (Return-val instr)) ")\n")]
    [(Run-method? instr)
     (string-append
      "(Run-method " (~v (Run-method-method instr)) " " (~v (Run-method-args instr)) " " (~v (Run-method-ret instr)) ")\n")]
    [else
     (string-append "TODO-" (~v instr) "\n")]))

(define (pretty-print-instr-list instr-list)
  ;; (display "printing instr list: ") (displayln instr-list)
  (define ret-str (void))
  (set! ret-str "")
  
  (cond
    [(empty? instr-list)
     (append-to ret-str "")]

    [(Single-branch? (first instr-list))
     (append-to ret-str (string-append
                         "(Single-branch " (pretty-print-expr (Single-branch-condition (first instr-list))) "\n"))

     (append-to ret-str
                (string-append
                 "(list\n" (pretty-print-instr-list (Single-branch-branch (first instr-list))) "\n))\n"))]

    [(Loop? (first instr-list))
     (append-to ret-str (string-append
                         "(Loop " (pretty-print-expr (Loop-condition (first instr-list))) "\n"))
     (append-to ret-str (string-append
                         "(list\n " (pretty-print-instr-list (Loop-instr-list (first instr-list))) "\n))\n"))]
    [else
     (append-to ret-str
                (string-append
                 (pretty-print-c-instr (first instr-list)) "\n"))]

    )

  (cond
    [(not (empty? instr-list))
     (append-to ret-str (pretty-print-instr-list (rest instr-list)))])
  ret-str)
    

(define (pretty-print-method m)
  (define ret-str (void))

  (set! ret-str (string-append

        "(Method \"" (Method-id m) "\" " (~v (Method-args m)) " \"" (Method-ret-type m) "\" \n(list\n"))


  (append-to ret-str (pretty-print-instr-list (Method-instr-list m)))


  (append-to ret-str "))\n")
  ret-str)



(define (pretty-print-library library)
  (reduce
   string-append
   (map
    pretty-print-method
    library)))
  
(define (create-sequential-search-sketch library size)
  (define prelude
    "#lang rosette
(require rosette/lib/synthax)
(require \"../program_representation/simulator-structures.rkt\")
(require \"../utilities/utilities.rkt\")
(require \"../examples/mooly-example.rkt\")
(require \"../examples/mooly-library.rkt\")
(require \"../cex_generalization/sequential-search.rkt\")

(require racket/string)

(require \"../cex_generalization/to-sketch.rkt\")
(require \"../cex_generalization/read-back-answer.rkt\")
(require \"../cex_generalization/metasketcher.rkt\")

(require \"../error_trace_generation/error-trace.rkt\")
(require (only-in \"../error_trace_generation/linearizable.rkt\" lin-result-trace))
(require (only-in \"../error_trace_generation/methods.rkt\" number-lines))
(require \"../optimal_cover/cover.rkt\" )\n")


(define ret-str (void))
(set! ret-str "")
(for ([i (range size)])
  (append-to ret-str (string-append "(define-symbolic meta" (~v i) " integer?)\n"))
  )

(string-append
 prelude
 ret-str
 "(define library (list \n"
 (pretty-print-library library)
 (generate-sequential-sketch library `() "int" size)

 "))\n"
 "
(define node1 (make-hash))
(hash-set! node1 \"key\" 1)
(hash-set! node1 \"val\" 5)
(hash-set! node1 \"next\" (None))

(define defined-structs (make-hash))
(hash-set! defined-structs \"Node\" (list \"throwaway\" \"key\" \"val\" \"next\"))

(displayln (interpret-method (sixth library) (list node1 1) defined-structs))

(display \"node1: \")
(displayln node1)
"

 ))


(displayln (create-sequential-search-sketch library 1))
;; (with-output-to-file "sketch_dump.rkt"
;;       (lambda () (printf 
;;                   (create-sequential-search-sketch library 1))))

;; (system "racket sketch_dump.rkt")
;; (system "rm -f sketch_dump.rkt")



;; External interpreter
(define (generate-final-sketcher-maker library arg-types ret-type size)
  (define ret-str (void))
  (set! ret-str "")
  (append-to ret-str "#lang rosette
(require rosette/lib/synthax)
")


  (append-to ret-str "#lang rosette\n(require rosette/lib/synthax)\n")
  
  (append-to ret-str (generate-sequential-sketch library arg-types ret-type size))
  (set! ret-str (string-append ret-str   (generate-library-code library)))    

)




(define (most-recent-binding env key)
  (cond
    [(hash-has-key? env key)
     (hash-ref env key)]
    [(hash-has-key? env "PREV_BINDINGS")
     (most-recent-binding (hash-ref env "PREV_BINDINGS") key)]
    [else
     (displayln "ERROR: Tried to access undeclared variable")
     (exit)]))

(define (set-most-recent-binding env key val)
  (cond
    [(hash-has-key? env key)
     (hash-set! env key val)]
    [(hash-has-key? env "PREV_BINDINGS")
     (set-most-recent-binding (hash-ref env "PREV_BINDINGS") key val)]
    [else
     (displayln "ERROR: Tried to set undeclared variable")
     (exit)]))
     


(define (interpret-method m arg-list all-defined-structs)
  (display "running... ") (displayln (Method-id m))
  (define env (void))
  (set! env (make-hash))

  (define (interpret-instruction instr)
    (displayln "instruction")
    (cond
      [(Create-var? instr)

       (hash-set! env (Create-var-id instr) (None))]

      [(Set-var? instr)
       (hash-set! env (Set-var-id instr) (interpret-expr (Set-var-assignment instr)))]

      [(Lock? instr)
       (void)]
      [(Unlock? instr)
       (void)]

      [(Set-pointer? instr)
       (let*
           ([id (Set-pointer-id instr)]
            [offset (Set-pointer-offset instr)]
            [val (Set-pointer-val instr)]
            [binding (most-recent-binding env id)])

         (if (hash? binding)
             (begin
               (hash-set! binding offset (interpret-expr val)))
             (begin
               (set-most-recent-binding env id (make-hash))
               (hash-set! (most-recent-binding env id) offset (interpret-expr val)))))]

      [(Return? instr)
       (hash-set! env "RETVAR" (interpret-expr (Return-val instr)))]

      [(Method-Choice? instr)
       (cond
         [(equal? (Method-Choice-meta-var instr) 1)
          (interpret-instruction (list-ref (Method-Choice-choice-list instr) 1))]
         [(equal? (Method-Choice-meta-var instr) 2)
          (interpret-instruction (list-ref (Method-Choice-choice-list instr) 2))]
         [(equal? (Method-Choice-meta-var instr) 3)
          (interpret-instruction (list-ref (Method-Choice-choice-list instr) 3))]
         [(equal? (Method-Choice-meta-var instr) 10)
          (interpret-instruction (list-ref (Method-Choice-choice-list instr) 10))]

         [(equal? (Method-Choice-meta-var instr) 0)
          (interpret-instruction (list-ref (Method-Choice-choice-list instr) 0))]
         [else
          (void)])]

      [(Run-method? instr)
       (let*
           ([method-called (Run-method-method instr)]
            [method-args (Run-method-args instr)]
            [method-ret (Run-method-ret instr)]
            [lib-method (get-lib-method library method-called)])
         (displayln "running new method")
         (interpret-method lib-method method-args all-defined-structs))]

         
       

      
      [else ;; Assume it is a run method
       (displayln "NEITHER")]



      ))

  (define (interpret-expr expr)
    (cond
      [(Equal? expr)
       (equal? (interpret-expr (Equal-expr1 expr)) (interpret-expr (Equal-expr2 expr)))]
      [(Not-equal? expr)
       (not (equal? (interpret-expr (Not-equal-expr1 expr)) (interpret-expr (Not-equal-expr2 expr))))]
      [(Or? expr)
       (or (interpret-expr (Or-expr1 expr)) (interpret-expr (Or-expr2 expr)))]
      [(And? expr)
       (and (interpret-expr (And-expr1 expr)) (interpret-expr (And-expr2 expr)))]
      [(or (number? expr) (boolean? expr) (hash? expr))
       expr]
      [(Not? expr)
       (not (interpret-expr (Not-expr expr)))]
      [(Is-none?? expr)
       ;; (display "is none check: ") (displayln (interpret-expr (Is-none?-val expr)))
       (None? (interpret-expr (Is-none?-val expr)))]

      [(Get-argument? expr)
       (interpret-expr (list-ref arg-list (Get-argument-id expr)))]

      [(Dereference? expr)
       ;; (display "dereference: ") (displayln expr)
       ;; (display "assignment: ") (displayln (hash-ref (most-recent-binding env (Dereference-id expr)) (Dereference-offset expr)))
       (hash-ref (most-recent-binding env (Dereference-id expr)) (Dereference-offset expr))]

      [(None? expr)
       (None)]

      [(New-struct? expr)
       (let*
           ([new-struct (make-hash)]
            [struct-type-name (New-struct-type expr)]
            [new-struct-arg-list (New-struct-arg-list expr)]
            [struct-attribute-list (hash-ref all-defined-structs struct-type-name)])

         ;; (display "size of struct arg list: ") (displayln (length new-struct-arg-list))
         ;; (displayln new-struct-arg-list)
         (for ([i (range ( - (length new-struct-arg-list) 1))])
           (hash-set! new-struct (list-ref struct-attribute-list i) (interpret-expr (list-ref new-struct-arg-list i))))

         new-struct)]

            

       

      [(Get-var? expr)
       (most-recent-binding env (Get-var-id expr))]
       
      [else
       (display "TODO: ") (displayln expr)
       #f]))


  (define (interpret-instr-list instr-list)
    (cond
      [(empty? instr-list) (void)]
      [(Single-branch? (first instr-list))
       ;; (display "branching on ") (displayln (Single-branch-condition (first instr-list)))
       (if (interpret-expr (Single-branch-condition (first instr-list)))
           (interpret-instr-list
            (append
             (Single-branch-branch (first instr-list))
             (rest instr-list)))
           (interpret-instr-list
            (rest instr-list)))]
      [(Loop? (first instr-list))
       (while
           (lambda () (interpret-expr (Loop-condition (first instr-list))))
         (lambda () (interpret-instr-list (Loop-instr-list (first instr-list)))))
       (interpret-instr-list
        (rest instr-list))]
      [else
       (interpret-instruction (first instr-list))
       (interpret-instr-list (rest instr-list))]))


  
  (let*
      ([m-instr-list (Method-instr-list m)])

    ;; (display (Method-id m)) (display " " ) (displayln arg-list)
    (interpret-instr-list m-instr-list))

  env)




  


;; (define node1 (make-hash))
;; (hash-set! node1 "key" 1)
;; (hash-set! node1 "val" 5)
;; (hash-set! node1 "next" (None))

;; (define defined-structs (make-hash))
;; (hash-set! defined-structs "Node" (list "throwaway" "key" "val" "next"))

;; (displayln (interpret-method (first library) (list node1 2 5) defined-structs))

;; (display "node1: ")
;; (displayln node1)


