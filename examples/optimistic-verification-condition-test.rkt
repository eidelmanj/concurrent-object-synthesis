 #lang racket
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



(define metasketch-lib ;; (metasketch-library-add-announcement library "extension"))
  library)
  ;; (modify-library-for-optimistic
  ;;  library
  ;;  "extension"
  ;;  (Hole 6 `() 7)))
  




;; Run Aliya's code to find error traces
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




(define results  err-traces)
(define result-trace-lists
  (map (lambda (t) (lin-result-trace t)) results))


;; Compute sets of pairs to be repaired
(define hole-set (optimal-cover (map lin-result-trace results) null))


;; Generate a sketch to guess optimistic conditions from one of the sets of pairs (first hole-set)
;; can't be repaired with optimistic concurrency
(define spit-out (optimistic-sketch-from-hole-list result-trace-lists (second hole-set) library))

(with-output-to-file "TMP.rkt"
  (lambda () (printf (first spit-out))))

;; Resolve sketch
(system "/u/eidelmanj/racket/bin/racket TMP.rkt > OUTPUT.txt")
(system "rm -f TMP.rkt")

;; Retrieve the appropriate optimistic conditions
(define optimistic-conditions
  (string->opts (file->string "OUTPUT.txt")))
(system "rm -f OUTPUT.txt")

;; Use the resolved sketch optimistic conditions to repair the library extension
(optimistic-repair
 (second spit-out) ;; All of the traces, with meta information
 optimistic-conditions ;; Results from the sketch
 library
 "extension")
