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



(define (convert-holes hole-list)
  (map (lambda (h) (Hole (hole-before h) (hole-interrupt h) (hole-after h))) hole-list))
     
      


(define hole-set (optimal-cover (map lin-result-trace results) null))

;; Only one hole set can be repaired with locks - TODO: make this check automatic
(define to-repair (second hole-set))
;; (displayln (second hole-set))
(minimal-lock (Method-instr-list (get-lib-method metasketch-lib "extension")) (convert-holes to-repair) `())
