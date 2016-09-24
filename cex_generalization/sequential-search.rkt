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




(define (generate-sequential-sketch library arg-types size)
  (define ret-str (void))
  (set! ret-str "")

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
  (append-to ret-str "))\n")

  ret-str
)
(displayln (generate-sequential-sketch library `() 3))
        
     

