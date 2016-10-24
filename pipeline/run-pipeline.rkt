#lang racket
(require "../program_representation/simulator-structures.rkt")
(require "../utilities/utilities.rkt")
(require "../examples/mooly-example.rkt")
(require "../examples/mooly-library.rkt")

(require racket/string)

(require "../cex_generalization/to-sketch.rkt")
(require "../cex_generalization/read-back-answer.rkt")
(require "../cex_generalization/metasketcher.rkt")
(require "../cex_generalization/translate-from-seq.rkt")

(require "../error_trace_generation/error-trace.rkt")
(require (only-in "../error_trace_generation/linearizable.rkt" lin-result-trace))
(require (only-in "../error_trace_generation/methods.rkt" number-lines))
(require "../optimal_cover/cover.rkt" )

(require "../internal_libs/map-library.rkt")

(require "../parser/parser.rkt")
(require         "../program_representation/racket-synthesis.rkt")

;;   (define prelude "
    
;; #lang rosette
;; (require rosette/lib/synthax)

;; (define current-thread (void))

;; (define method-exit (void))
;; (set! method-exit #f)


;; (define lock-list (void))
;; (set! lock-list (list))



;; (define (has-lock t-id lock)
;;   (cond
;;     [(> lock (- (length lock-list) 1))
;;      (set! lock-list (append lock-list (list t-id)))
;;      #t]
;;     [else
;;      (or (> 0 (list-ref lock-list lock)) (equal? t-id (list-ref lock-list lock)))]))



;; (define (replace-lock lock t-id)
;;   (cond
;;     [(equal? lock 0)
;;      (append (list t-id) (rest lock-list))]
;;     [else
;;      (append (list (first lock-list)) (replace-lock (- lock 1) t-id))]))

;; (define (get-lock t-id lock)
;;   (set! lock-list (replace-lock lock t-id)))

;; (define (release-lock t-id lock)
;;   (set! lock-list (replace-lock lock -1)))


;; (struct Node (next key val bits) #:mutable)
;; (struct List (first last) #:mutable)

;; (define shared (void))
;; (struct None ())
;; (set! shared (Node  (None) \"test\" \"testval\" (None)))


;; (define first-args (void))
;; (set! first-args (list shared 1))
;; (define POSSIBLE (void))
;; (set! POSSIBLE #t)

;; (define TRACE-TYPE (void))
;; (set! TRACE-TYPE \"no-optimistic-restart\")



;; (define RETURN-VAL (void))
;; (define TMP-RET (void))
;; (define-symbolic meta-var1 boolean?) ;; TODO collect meta-vars
;; (define OPTIMISTIC (void)) ;; TODO Need to do this automatically

;; (define-symbolic pick-trace integer?)")

;; (displayln prelude)
;; (displayln (generate-library-code map-library))

;; (exit)

(define all-args  (vector->list (current-command-line-arguments)))

(cond
  [(not (= (length all-args) 3))
      (displayln "Usage: run-pipeline.rkt extension-file sketch-background-file concurrent-lib")
      (exit)])


(define extension-file (first all-args))
(define sketch-background-file (second all-args))
(define concurrent-lib (third all-args))


(displayln sketch-background-file)
(define use-map (void))
(set! use-map false)

(set! use-map (equal? concurrent-lib "-map"))



(define extension-contents (file->lines extension-file))
(define extension-no-meta-info (rest extension-contents))


(define concurrent-lib-str
  (if use-map
      "MAP"
      (file->string concurrent-lib)))



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




;; (define extension-no-first-line-str (retrieve-sketch "outComplete2.cpp"))
(define extension-no-first-line-str (rebuild-string (rest extension-contents)))


(define process
  (begin
    (system "rm -f ../c++/test/*")
    (system "rm -f ../c++/test/original.txt")
    (system "rm -f ../c++/outComplete*.cpp")
    (system "rm -f ./outComplete*.cpp")
    (with-output-to-file "../c++/test/original.txt"
      (lambda () (printf extension-no-first-line-str)))

    (system (string-append "cp " sketch-background-file " ../c++/background.sk"))
    (system (string-append "cp " extension-file " ../c++/extension.c"))


    (system (string-append "cd ../c++ && cat extension.c | ./seq_search"))
    
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
;; (define solution-contents
;;   "
;; void copySKETCH(List* l1, List* l2, int z0_0, int& _out) {
;;   _out = 0;
;;   int  z3_s74=0;
;;   get(l1, z0_0, z3_s74);
;;   int  z4_s166=0;
;;   get(l2, z3_s74, z4_s166);
;;   if ((z3_s74) == (100)) {
;;     _out = 100;
;;     return;
;;   }
;;   if ((z4_s166) == (-(100))) {
;;     int  z5_s354=0;
;;     push(l2, z3_s74, z4_s166, z5_s354);
;;   }
;;   _out = z4_s166;
;;   return;
;; }
;; ")




(define library
  (cond
    [(not use-map)
     (define concurrent-lib-contents (file->string concurrent-lib))


     ;; Get rid of address marker which our parser doesn't understand
     (define lib-str (string-replace (string-append concurrent-lib-contents "\n" solution-contents) "&" ""))


     (define parsed-lib-unprocessed
       (let*
           ((test-program lib-str)
            (input (open-input-string test-program)))
         (translate (simple-math-parser (lex-this simple-math-lexer input)))))


     (repair-all-methods parsed-lib-unprocessed)]
    [else
     (define lib-str
       (string-replace
        (string-replace
         (string-replace
          (string-replace solution-contents "&" "")
          "(-(100))"
          "NULL")
         "-100"
         "NULL")
        "-(100)"
        "NULL"))
        

     (displayln lib-str)
     (define parsed-lib-unprocessed
       (let*
           ((test-program lib-str)
            (input (open-input-string test-program)))
         (translate (simple-math-parser (lex-this simple-math-lexer input)))))

     (define repaired-extension (repair-all-methods parsed-lib-unprocessed))


     
     (append repaired-extension map-library)]))






;; (displayln solution-contents)

;; (displayln (file->string "../c++/test/out2.txt"))


(displayln  (get-lib-method library "copySKETCH"))
(map displayln

 (Method-instr-list (get-lib-method library "copySKETCH")))

 

;; (map
;;  (lambda (m) (displayln (generate-library-code (list m))))
;;  library)

(map
 displayln
 (Method-instr-list (get-lib-method library "copySKETCH")))

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
     ,(Run-method "push" `(,(Get-var "shared1") 1 2) null)
     ,(Run-method "push" `(,(Get-var "shared1") 1 5) null)
     ,(Run-method "push" `(,(Get-var "shared1") 2 7) null)
     ,(Run-method "push" `(,(Get-var "shared2") 2 4) null))))






(define results  err-traces)
(define result-trace-lists
  (map (lambda (t) (lin-result-trace t)) results))





(cond
  [(empty? results)
    (displayln "Method is linearizable!")
    (exit)]
   [else
    (displayln "Not empty!")])


;; Compute sets of pairs to be repaired
(define hole-set (optimal-cover (map lin-result-trace results) null))


(define (contains-invalid-hole hole-list)
  (> (length
      (filter
      (lambda (a)
        (null? (hole-after a)))
      hole-list))
     0))

(define filtered-hole-set
  (filter (lambda (l) (not (contains-invalid-hole l))) hole-set))



(map
 (lambda (e) (displayln e) (displayln "----------------------"))
 err-traces)


(map
 (lambda (e) (displayln e) (displayln "----------------------"))
 filtered-hole-set)


(map
 (lambda (e) (displayln e) (displayln "----------------------"))
 hole-set)


(define (convert-holes hole-list)
  (map (lambda (h) (Hole (hole-before h) (hole-interrupt h) (hole-after h))) hole-list))





;; (displayln spit-out)


(define (locked-solution h)
  (minimal-lock (Method-instr-list (get-lib-method library "copySKETCH")) (convert-holes (first filtered-hole-set)) `()))


(define (optimistic-solution h)
  ;; (displayln solution-contents)
  
  (define spit-out (optimistic-sketch-from-hole-list result-trace-lists h library))  
  (system "rm -f sketch-dump.rkt")
  (with-output-to-file "sketch-dump.rkt"
    (lambda () (printf (first spit-out))))
  
  ;; Resolve sketch
  (define throwout (system "racket sketch-dump.rkt > OUTPUT.txt"))
  ;; (define throwout2 (system "rm -f sketch-dump.rkt"))
  
  ;; Retrieve the appropriate optimistic conditions
  (define optimistic-conditions
    (string->opts (file->string "OUTPUT.txt")))
  ;; (define throwout3 (system "rm -f OUTPUT.txt"))
  
  
  ;; (displayln optimistic-conditions)
  (displayln "-----------------------")
  
  ;; Use the resolved sketch optimistic conditions to repair the library extension
   (optimistic-repair
              (second spit-out) ;; All of the traces, with meta information
              optimistic-conditions ;; Results from the sketch
              library
              "copySKETCH"))
  






    

(define solution-set
  (reduce
   append
   (map
    (lambda (h)

      (append

       (list (locked-solution (rest h)))
       (list (optimistic-solution (list (first h))))))

    filtered-hole-set)))


(displayln (second solution-set))

(define merged-set
  (reduce
   append
   (map
    (lambda (h)
      (displayln (connect-solutions-end-to-end (second solution-set)
                                               (locked-solution  (rest h))
                                               (first h)
                                               (second h)
                                               1
                                               0)))
    filtered-hole-set)))







