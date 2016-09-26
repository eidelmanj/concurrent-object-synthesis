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


;; I feel like this must be defined somewhere else, but for now:
(define all-types (list "int" "char"))


(define-syntax-rule (append-to str new-str)
  (set! str (string-append str new-str)))



(define (generate-sequential-sketch-line library arg-types tmp-id)
  (define ret-str (void))
  (set! ret-str "")


  (append-to ret-str "(choose ")


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

  (append-to ret-str ")\n")
  
  ret-str)




(define (generate-sequential-sketch library arg-types ret-type size)
  (define ret-str (void))
  (set! ret-str "")

  (append-to ret-str (string-append "(Method \"EXTSKETCH\" " (~v arg-types) " \""  ret-type "\" \n"))
  (append-to ret-str "(list \n")

  (for ([i (range size)])
    (append-to ret-str (string-append "(Create-var tmp" (~v i) " \"int\")\n"))
    )

  (for ([i (range size)])
    (append-to ret-str (generate-sequential-sketch-line library arg-types i))
    )


  (append-to ret-str "(Return (Choose " )
  (for ([i (range size)])
    (append-to ret-str (string-append "tmp" (~v i) " "))
    )
  (append-to ret-str "))))\n")



  ret-str
)
(displayln (generate-sequential-sketch library `() "int" 3))


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
      "(Get-var " (Get-var-id expr) ")")]
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
    [else
     "TODO - " (~v expr) " "]))
       

(define (pretty-print-c-instr instr)
  (cond
    [(not (C-Instruction? instr))
     "Warning: Not C instr\n"]
    [(Set-pointer? instr)
     (string-append "(Set-pointer " (Set-pointer-id instr) " " (Set-pointer-type instr)
                    " " (Set-pointer-offset instr) " " (Set-pointer-val instr) ")\n")]
    [(CAS? instr)
     (string-append "(CAS " (CAS-v1 instr) " " (CAS-v2 instr) " " (CAS-new-val instr) " " (CAS-ret instr) ")\n")]
    [(Create-var? instr)
     (string-append "(Create-var " (Create-var-id instr) " " (Create-var-type instr) ")\n")]
    [(Set-var? instr)
     (string-append
      "(Set-var " (Set-var-id instr) " " (Set-var-assignment instr) ")\n")]
    [(Lock? instr)
     (string-append
      "(Lock " (Lock-id instr) ")\n")]
    [(Unlock? instr)
     (string-append
      "(Unlock " (Unlock-id instr) ")\n")]
    [(Return? instr)
     (string-append
      "(Return " (Return-val instr) ")\n")]
    [(Run-method? instr)
     (string-append
      "(Run-method " (Run-method-method instr) " " (~v (Run-method-args instr)) " " (Run-method-ret instr) ")\n")]
    [else
     (string-append "TODO-" (~v instr) "\n")]))

(define (pretty-print-instr-list instr-list)
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

    ;; TODO: Branches, loops
    [else
     (append-to ret-str
                (string-append
                 (pretty-print-c-instr (first instr-list)) "\n"))]

    ))
    

(define (pretty-print-method m)
  (define ret-str (void))
  (set! ret-str (string-append

        "(Method \"" (Method-id m) "\" " (Method-args m) " \"" (Method-ret-type m) "\" \n(list\n"))

  (append-to ret-str (pretty-print-instr-list m))


  (append-to ret-str "))\n")
  ret-str)

  


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
