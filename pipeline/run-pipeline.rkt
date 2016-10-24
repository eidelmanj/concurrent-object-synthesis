#lang racket
(require "../program_representation/simulator-structures.rkt")
(require "../utilities/utilities.rkt")
(require "../examples/mooly-example.rkt")
(require "../examples/mooly-library.rkt")

(require racket/string)

(require "../cex_generalization/to-sketch.rkt")
;; (require "../cex_generalization/read-back-answer.rkt")
(require "../cex_generalization/metasketcher.rkt")
(require "../cex_generalization/translate-from-seq.rkt")

(require "../error_trace_generation/error-trace.rkt")
(require (only-in "../error_trace_generation/linearizable.rkt" lin-result-trace))
(require (only-in "../error_trace_generation/methods.rkt" number-lines))
(require "../optimal_cover/cover.rkt" )


(require "../parser/parser.rkt")
(require         "../program_representation/racket-synthesis.rkt")


(define all-args  (vector->list (current-command-line-arguments)))

(cond
  [(not (= (length all-args) 3))
      (displayln "Usage: run-pipeline.rkt extension-file sketch-background-file concurrent-lib")
      (exit)])


(define extension-file (first all-args))
(define sketch-background-file (second all-args))
(define concurrent-lib (third all-args))


(define extension-contents (file->lines extension-file))
(define extension-no-meta-info (rest extension-contents))

(define concurrent-lib-str (file->string concurrent-lib))


(define (rebuild-string lines)
  (reduce
   (lambda (a b) (string-append a "\n" b))
   lines))
   

(define (find-sketch-beginning lines)
  (cond
    [(empty? lines) `()]
    [(and
      (regexp-match-positions #rx"void" (first lines))
      (regexp-match-positions #rx"SKETCH" (first lines)))
     lines]
    [else
     (find-sketch-beginning (rest lines))]))

(define (until-end-of-sketch lines counter)
  (cond
    [(empty? lines) `()]
    [(regexp-match-positions #rx"{" (first lines))
     ;; (display "adding 1 to ")(displayln counter)
     (append
      (list (first lines))
      (until-end-of-sketch (rest lines) (+ counter 1)))]
    [(and
      (regexp-match-positions #rx"}" (first lines))
      (= counter 1))
     ;; (displayln "finished method!")

      (list (first lines))]
    [(regexp-match-positions #rx"}" (first lines))
     ;; (display "subtracting 1 from ") (displayln counter)
     (append
      (list (first lines))
      (until-end-of-sketch (rest lines) (- counter 1)))]
    [else
     (append
      (list (first lines))
      (until-end-of-sketch (rest lines) counter))]))



(define (retrieve-sketch filename)
  (rebuild-string (until-end-of-sketch (find-sketch-beginning (file->lines filename)) 0)))




(define extension-no-first-line-str (retrieve-sketch "outComplete2.cpp"))

(define process
  (begin
    (system "rm -f ../c++/test/original.txt")
    (with-output-to-file "../c++/test/original.txt"
      (lambda () (printf extension-no-first-line-str)))

    (system (string-append "cp " sketch-background-file " ../c++/background.sk"))


    ;; (system (string-append "cd ../c++ && cat " extension-file " | ./seq_search > ../pipeline/out_log.txt"))
    
    (system "cp ../c++/outComplete*.cpp ./")
    ))


(define (highest-outComplete-file l cur-highest)
  (cond
    [(empty? l) (string-append "outComplete" (~v cur-highest) ".cpp")]
    [(> (string->number (string (string-ref (first l) 11))) cur-highest)
     (highest-outComplete-file (rest l) (string->number (string (string-ref (first l) 11))))]
    [else
     (highest-outComplete-file (rest l) cur-highest)]))
     

(define all-files
  (filter
   (lambda (x) (regexp-match-positions #rx"outComplete" x))
   
   (map
    path->string
    (directory-list "./"))))

(define solution-file (highest-outComplete-file all-files 0))


(define solution-contents (retrieve-sketch solution-file))
(define concurrent-lib-contents (file->string concurrent-lib))


;; Get rid of address marker which our parser doesn't understand
(define lib-str (string-replace (string-append concurrent-lib-contents "\n" solution-contents) "&" ""))


(define parsed-lib-unprocessed
  (let*
      ((test-program lib-str)
       (input (open-input-string test-program)))
    (translate (simple-math-parser (lex-this simple-math-lexer input)))))

(define library
  (repair-all-methods parsed-lib-unprocessed))



;; (map
;;  (lambda (m) (displayln (generate-library-code (list m))))
;;  library)

;; (map
;;  displayln
;;  (Method-instr-list (get-lib-method library "add")))

;; Run Aliya's code to find error traces
;; For now  initial values are preset
(bound 1)
(define-values (err-traces numbered-method)
  (error-traces
   library
   "copySKETCH"

   `(,(Create-var "shared1" "List")
     ,(Create-var "shared2" "List")
     ,(Create-var "primitive" "int")
     ,(Set-var "shared1" (New-struct "List" `(,(None) (None))))
     ,(Set-var "shared2" (New-struct "List" `(,(None) (None))))
     ,(Run-method "add" `(,(Get-var "shared1") 1) null)
     ,(Run-method "add" `(,(Get-var "shared1") 2) null))))







(map
 (lambda (e) (displayln e) (displayln "----------------------"))
 err-traces)
