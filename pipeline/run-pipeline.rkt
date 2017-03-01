#lang racket

(require "../program_representation/simulator-structures.rkt")

         

(require "../utilities/utilities.rkt")
(require "../examples/mooly-example.rkt")
(require "../examples/mooly-library.rkt")
(require "../cex_generalization/to-c-program.rkt")


(require racket/string)

;; (require "../cex_generalization/to-sketch.rkt")

;; (require "../cex_generalization/read-back-answer.rkt")
(require (only-in "../cex_generalization/read-back-answer.rkt"
         string->opts))
;; (require "../cex_generalization/metasketcher.rkt")
(require (only-in "../cex_generalization/metasketcher.rkt"
                  minimal-lock
                  optimistic-sketch-from-hole-list
                  optimistic-repair))

;; (require "../cex_generalization/translate-from-seq.rkt")
(require (only-in  "../cex_generalization/translate-from-seq.rkt"
                   repair-all-methods
                   simplify-redundant-method))

;; (require "../error_trace_generation/error-trace.rkt")
(require (only-in "../error_trace_generation/error-trace.rkt"
                  bound
                  max-client-length
                  error-traces))

(require (only-in "../error_trace_generation/linearizable.rkt" lin-result-trace lin-result-correct-results))
(require (only-in "../error_trace_generation/methods.rkt" number-lines))
(require "../optimal_cover/cover.rkt" )
;; (require (only-in "../optimal_cover/cover.rkt")
;;          optimal-cover)

;; (require "../internal_libs/map-library.rkt")
(require (only-in "../internal_libs/map-library.rkt"
                  map-library))
(require (only-in "../internal_libs/inc-library.rkt"
                  inc-library))
(require (only-in "../internal_libs/stack-library.rkt"
                  stack-library))
(require (only-in "../internal_libs/queue-library.rkt"
                  queue-library))

;; (require "../internal_libs/inc-library.rkt")
;; (require "../internal_libs/stack-library.rkt")
;; (require "../internal_libs/queue-library.rkt")

;; (require "../parser/parser.rkt")
(require (only-in "../parser/parser.rkt"
                  simple-math-parser
                  lex-this
                  simple-math-lexer))
;; (require         "../program_representation/racket-synthesis.rkt")
(require (only-in "../program_representation/racket-synthesis.rkt"
                  translate))

(require (only-in "sketch-parser.rkt"
                  rebuild-string
                  retrieve-sketch))

(define all-args  (vector->list (current-command-line-arguments)))


;; Parse command line arguments
(cond
  [(not (= (length all-args) 5))
      (displayln "Usage: run-pipeline.rkt extension-file sketch-background-file concurrent-lib num_shared num_args")
      (exit)])


(define extension-file (first all-args))
(define sketch-background-file (second all-args))
(define concurrent-lib (third all-args))

(define num-shared (fourth all-args))
(define num-args (fifth all-args))



(define use-map (void))
(set! use-map false)


(define use-inc (void))
(set! use-inc false)

(define use-stack (void))
(set! use-stack false)

(define use-queue (void))
(set! use-queue false)

(set! use-map (equal? concurrent-lib "-map"))
(set! use-inc (equal? concurrent-lib "-inc"))
(set! use-stack (equal? concurrent-lib "-stack"))
(set! use-queue (equal? concurrent-lib "-queue"))



(define extension-contents (file->lines extension-file))
(define extension-no-meta-info (rest extension-contents))


;; (define concurrent-lib-str
;;   (if (or use-map use-inc use-stack use-queue)
;;       "MAP"
;;       (file->string concurrent-lib)))

;; Eventually I will use this to have custom user-built libraries
(define concurrent-lib-str "TODO")






;; (define extension-no-first-line-str (retrieve-sketch "outComplete2.cpp"))
(define extension-no-first-line-str (rebuild-string (rest extension-contents)))

(define seq-time-start (current-inexact-milliseconds))
(define test-dir (build-path (find-system-path 'home-dir) ".concurrent_obj"))
(define inner-test-dir (build-path (find-system-path 'home-dir) ".concurrent_obj" "test"))

(define process

  (begin


    (if (directory-exists? test-dir)
        (begin (void) )
        (system (string-append "mkdir " (path->string test-dir))))

    (if (directory-exists? inner-test-dir)
        (begin (void) )
        (system (string-append "mkdir " (path->string inner-test-dir))))

    (system (string-append "rm -f " (path->string test-dir) "/*"))
    (system (string-append "cp ../c++/run_test.sh " (path->string test-dir)))
    (system (string-append "rm -f " (path->string test-dir) "/test/*")) ;; ../c++/test/*")
    ;; (system (string-append "rm -f " (path->string test-dir) " ;; ../c++/test/original.txt")
    (system (string-append "rm -f " (path->string test-dir) "/outComplete*.cpp"));; ../c++/outComplete*.cpp")
    (system "rm -f  ./outComplete*.cpp")

    (with-output-to-file (string-append (path->string test-dir) "/test/original.txt")
      (lambda () (printf extension-no-first-line-str)))

    (system (string-append "cp " sketch-background-file " " (string-append (path->string test-dir)
                                                                       "/background.sk")))
    (system (string-append "cp " extension-file " " (string-append (path->string test-dir)
                                                               "/extension.c")))


    (system (string-append
            "cat " (path->string test-dir) "/extension.c | ../c++/seq_search " num-shared " " num-args " "
                           (if use-map
                               "0"
                               "1")))
    

    ))


(define seq-time-end (current-inexact-milliseconds))

(define seq-time (- seq-time-end seq-time-start ))

(define (highest-outComplete-file l cur-highest)
  (cond
    [(empty? l) (string-append "outComplete" (~v cur-highest) ".cpp")]
    [
     (and
      (> (string->number (string (string-ref (first l) 11))) cur-highest)
      (regexp-match-positions #rx".cpp" (first l)))
     (highest-outComplete-file (rest l) (string->number (string (string-ref (first l) 11))))]
    [else
     (highest-outComplete-file (rest l) cur-highest)]))
     

(define all-files
  (filter
   (lambda (x) (regexp-match-positions #rx"outComplete" x))
   
   (map
    path->string
    (directory-list (path->string test-dir)))))

(define solution-file (highest-outComplete-file all-files 0))


(define solution-contents (retrieve-sketch
                           (string-append
                            (path->string test-dir)
                            "/"
                            solution-file)))
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
    [(and (not use-map) (not use-inc) (not use-stack) (not use-queue))
     (define concurrent-lib-contents (file->string concurrent-lib))


     ;; Get rid of address marker which our parser doesn't understand
     (define lib-str (string-replace (string-append concurrent-lib-contents "\n" solution-contents) "&" ""))


     (define parsed-lib-unprocessed
       (let*
           ((test-program lib-str)
            (input (open-input-string test-program)))
         (translate (simple-math-parser (lex-this simple-math-lexer input)))))


     (repair-all-methods parsed-lib-unprocessed)]
    [use-map
     (define lib-str
       (string-replace
        (string-replace
         (string-replace
          (string-replace solution-contents "int&" "int")
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


     
     (append repaired-extension map-library)]


    [use-inc
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


     
     (append repaired-extension inc-library)]
    [use-stack
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


     
     (append repaired-extension stack-library)]

    [use-queue
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


     
     (append repaired-extension queue-library)]


    ))







;; (displayln solution-contents)

;; (displayln (file->string "../c++/test/out2.txt"))


;; (displayln  (get-lib-method library "copySKETCH"))
;; (map displayln

;;  (Method-instr-list (get-lib-method library "copySKETCH")))

 

;; (map
;;  (lambda (m) (displayln (generate-library-code (list m))))
;;  library)

;; (map
;;  displayln
;;  (Method-instr-list (get-lib-method library "copySKETCH")))

;; Run Aliya's code to find error traces
;; For now  initial values are preset




(define error-time-start (current-inexact-milliseconds))

(define-values (err-traces numbered-method)
  (cond
    [use-inc
     (bound 3)
     (max-client-length 3)

     (error-traces
      library
      "copySKETCH"

      `(, (Create-var "shared1" "Shared_int")
          ,(Set-var "shared1" (New-struct "Shared_int" `(, 2)))
          ))]
          ;; (Set-pointer "shared1" "Shared_int" "val" 0)))]




    [use-stack

     (bound 1)
     (max-client-length 5)

     (error-traces
      library
      "copySKETCH"
            
      `(,(Create-var "shared1" "Stack")
      
        ;; ,(Create-var "primitive" "int")
        ,(Set-var "shared1" (New-struct "Stack" `(,(None))))
      
        ,(Run-method "push" `(,(Get-var "shared1") 7) null)
        ,(Run-method "push" `(,(Get-var "shared1") 5) null)))]

    [use-queue
     
     (bound 3)
     (max-client-length 5)

     (error-traces
      library
      "copySKETCH"
            
      `(,(Create-var "shared1" "Queue")
      
        ;; ,(Create-var "primitive" "int")
        ,(Set-var "shared1" (New-struct "Queue" `(,(None))))
      
        ,(Run-method "enq" `(,(Get-var "shared1") 7) null)
        ,(Run-method "enq" `(,(Get-var "shared1") 5) null)))]




     [else
     (bound 1)
     (max-client-length 2)
     
     (error-traces
      ;; search-traces
      library
      "copySKETCH"
      
      `(,(Create-var "shared1" "List")
        ,(Create-var "shared2" "List")
        ,(Create-var "primitive" "int")
        ,(Set-var "shared1" (New-struct "List" `(,(None) (None))))
        ,(Set-var "shared2" (New-struct "List" `(,(None) (None))))
        ;; ,(Run-method "push" `(,(Get-var "shared1") 1 2) null)
        ;; ,(Run-method "push" `(,(Get-var "shared1") 1 5) null)
        ;; ,(Run-method "push" `(,(Get-var "shared2") 1 6) null)
        ,(Run-method "push" `(,(Get-var "shared1") 2 7) null)
        ,(Run-method "push" `(,(Get-var "shared2") 2 4) null)))]))
   



(define error-time-end (current-inexact-milliseconds))
(define error-time (- error-time-end error-time-start))


(define results  err-traces)



(define result-trace-lists
  (map (lambda (t) (lin-result-trace t)) results))





(cond
  [(empty? results)
    (displayln "Method is linearizable!")
    (exit)]
   [else
    (displayln "Not empty!")])

;; err-traces numbered-method mut-args client

(display "err-traces: ") (map displayln err-traces)
;; (display "mut-args: ") (displayln 


(define hole-time-start (current-inexact-milliseconds))
;; Compute sets of pairs to be repaired
(define hole-set (optimal-cover (map lin-result-trace results) null))
(map displayln results)
(map displayln hole-set)

(define hole-time-end (current-inexact-milliseconds))
(define hole-time (- hole-time-end hole-time-start))


(define (next-method instr-list)
  (cond
    [(empty? instr-list) (displayln "ERROR: next method failed") (exit)]
    [(Single-branch? (first instr-list))
     (next-method (append
                   (Single-branch-branch (first instr-list))
                   (rest instr-list)))]
    [(Run-method? (first instr-list))
     (C-Instruction-instr-id (first instr-list))]
    [else
     (next-method (rest instr-list))]))


;; (define (remove-duplicate-holes h-list)
;;   (cond
;;     [(> 2 (length h-list)) h-list]
;;     [(and
;;       (equal?
;;        (hole-before (first h-list))
;;        (hole-before (second h-list)))
;;       (equal? 
;;        (hole-interrupt (first h-list))
;;        (hole-interrupt (second h-list)))
;;       (equal?
;;        (hole-after (first h-list))
;;        (hole-after (second h-list))))
;;      (append
;;       (list (first h-list))
;;       (rest (rest h-list)))]
;;     [else
;;      (append
;;       (list (first h-list))
;;       (remove-duplicate-holes (rest h-list)))]))

(define (hole-equality h1 h2)

  (and (equal? (hole-before h1) (hole-before h2))
       (equal? (hole-after h1) (hole-after h2))
       (equal? (hole-interrupt h1) (hole-interrupt h2))))

      
    
(define (correct-hole h instr-list prev-method)
  ;; (display "correcting hole: ") (displayln instr-list)
  ;; (display "finding after: ") (displayln (hole-after h))

  (cond
    [(empty? instr-list)
      h ]
    [(Single-branch? (first instr-list))
     (correct-hole h (append (Single-branch-branch (first instr-list)) (rest instr-list)) prev-method)]
    [(and
      (Run-method? (first instr-list))
     ;; (displayln (C-Instruction-instr-id (first instr-list)))
     ;; (displayln (equal? 17 (hole-after h)))
     ;; (exit)
      (equal?
       (C-Instruction-instr-id (first instr-list))
       (hole-after h))
     ;; (exit)
      (equal? (Run-method-method (first instr-list))
              "compute"))

     ;; (display "run-method?") (displayln (Run-method? (first instr-list)))
     ;; (display "instr-id: ") (displayln (C-Instruction-instr-id (first instr-list)))
     ;; (display "hole-after: ") (displayln (hole-after h))
     ;; (display "instr-id == holafter: ") (displayln (equal?
     ;;                                                (C-Instruction-instr-id (first instr-list))
     ;;                                                (hole-after h)))

     (displayln "found a compute!")
     ;; (correct-hole h (rest instr-list))]
     
     (hole (hole-before h) (hole-interrupt h) (next-method (rest instr-list)))]


    [(and
      (Run-method? (first instr-list))

      (equal?
       (C-Instruction-instr-id (first instr-list))
       (hole-before h))

      (equal? (Run-method-method (first instr-list))
              "compute"))


     (displayln "found a compute!")
     ;; (correct-hole h (rest instr-list))]
     
     (hole prev-method (hole-interrupt h) (hole-after h))]

    [(Run-method? (first instr-list))

     (correct-hole h (rest instr-list) (C-Instruction-instr-id (first instr-list)))]

    
    [else

     ;; (display "correcting hole: ") (displayln instr-list)
     (correct-hole h (rest instr-list) prev-method)]))
    

(define (correct-all-holes h-list)
  (map (lambda (h) (correct-hole h (Method-instr-list (get-lib-method library "copySKETCH"))
                                 null))
               
       h-list))



(define (contains-invalid-hole hole-list)
  (> (length
      (filter
      (lambda (a)
        (null? (hole-after a)))
      hole-list))
     0))

(define filtered-hole-set
  (map
   (lambda (h-list)
     (unique
      hole-equality
      h-list))
   (map correct-all-holes (filter (lambda (l) (not (contains-invalid-hole l))) hole-set))))

(display "filtered holes: ") (displayln filtered-hole-set)

;; (exit)
(define error-trace-num (length err-traces))
(define num-hole-sets (length filtered-hole-set))

;; (map
;;  (lambda (e) (displayln e) (displayln "----------------------"))
;;  err-traces)


;; (map
;;  (lambda (e) (displayln e) (displayln "----------------------"))
;;  filtered-hole-set)


;; (map
;;  (lambda (e) (displayln e) (displayln "----------------------"))
;;  hole-set)


(define (convert-holes hole-list)
  (map (lambda (h) (Hole (hole-before h) (hole-interrupt h) (hole-after h))) hole-list))





;; (displayln spit-out)

(define (find-announcement-answer lines)
  (define (get-sketch-start lines)
    (cond
      [(empty? lines) `()]
      [(regexp-match #rx"void copySKETCH" (first lines))
       (rest lines)]
      [else
       (get-sketch-start (rest lines))]))

  (define (extract-sketch-answer-line lines)
    (cond
      [(empty? lines) `()]
      [(regexp-match #rx"if \\(\\(remote_val" (first lines))
       (string-replace (last (string-split (third lines) " ")) ";" "")]
      [else
       (extract-sketch-answer-line (rest lines))]))

  (extract-sketch-answer-line (get-sketch-start lines)))


(define (bad-value str)
  (match str
    ["dec" "0"]
    [_ "-100"]))

(define (solve-announcement h)
  
  (define background-str
    (string-replace
     (file->string sketch-background-file)
     "assert"
     "if (t1 != -10 && t2 != -10) assert"))
  

  (define sketch-method

    (method->c (get-lib-method library "copySKETCH")
               "lock"
               `()
               (hole-before h)
               #t)
    )

  (cond
     
    [(or
      (equal? (hole-interrupt h)
              "dec")
      (equal? (hole-interrupt h)
              "deq")
      (equal? (hole-interrupt h)
              "pop"))


     (define new-sketch-method

       (string-replace
        (string-append sketch-method
                       "int composed_val = ??;\nint remote_val = " (hole-interrupt h) "(a0);\n"
                       "if (remote_val == "(bad-value (hole-interrupt h))") {"
                       "NOT_ZERO = NOT_ZERO + 1;\n"
                       "return composed_val;\n"
                       "}"
                       "else return -10;\n}\n")
        "void"
        "int"))
     
     
     
     (define non-sketch-method
       (string-replace
        (string-replace
         
         (string-append
          (method->c (get-lib-method library "copySKETCH")
                     "lock"
                     `()
                     `()
                     #t)
          "int remote_val = " (hole-interrupt h)"(a0);\n"
          "if (remote_val == "(bad-value (hole-interrupt h))") return composed_val;\n"
          "else return -10;\n}\n")
         
         "copySKETCH"
         "copy")
        "void"
        "int"))
     
     
     
     
     
     ;; (displayln new-sketch-method)
     ;; (displayln non-sketch-method)
     
     (define new-background-unedited


        (string-replace
         (string-replace background-str "ORIGINAL" non-sketch-method)
         "SKETCH\n"
         new-sketch-method))

     (define new-background

       (string-append
        (substring new-background-unedited 0 (- (string-length new-background-unedited) 1))
        "assert(NOT_ZERO > 0);\n }\n"))

     
     (system "rm -f ../c++/SKETCH_ANNOUNCE")
     (system "rm -f ../c++/SKETCH_ANNOUNCE.cpp")
     
     (with-output-to-file "../c++/SKETCH_ANNOUNCE"
       (lambda () (printf (string-replace
                           new-background
                           "Node TMP-PTR;"
                           ""

                           ))))

     (system "cd ../c++ && ./run_test.sh SKETCH_ANNOUNCE")

     (define sketch_response
       (with-handlers ([exn:fail? (lambda (exn) #f)])
         (file->lines "../c++/SKETCH_ANNOUNCE.cpp")))

     (define announce-return
       (if sketch_response
           (find-announcement-answer sketch_response)
           #f))


     


     
     announce-return]
    [else
     #f]))


     

    ;;  ]
    ;; [else
    ;;  (None)]))





(define (generate-announcement-solution instr-list h val)
  (cond
    [(empty? instr-list) `()]
    [(and
      (C-Instruction? (first instr-list))
      (equal?
       (C-Instruction-instr-id (first instr-list))
       (hole-before h)))

     (append
      (list (first instr-list))
      (list (Announce-Check val))
       (rest instr-list))]
    [else
     (append
      (list (first instr-list))
      (generate-announcement-solution (rest instr-list) h val))]))
            
     
        
  

(define (announcement-solution h)
  (define announce-val (solve-announcement (first h)))

  (if announce-val
      (generate-announcement-solution (Method-instr-list (get-lib-method library "copySKETCH")) (first h) announce-val)
      #f)
  
)

(define (locked-solution h instr-list)
  (define lock-time-start (current-inexact-milliseconds))
  (define lock-answer
    (minimal-lock instr-list (convert-holes h) `()))
  (define lock-time-end (current-inexact-milliseconds))

  (display "lock time: ") (displayln (- lock-time-end lock-time-start))
  lock-answer)
  


(define (is-writer m-call)
  (cond
    [(equal? m-call "push") #t]
    [(equal? m-call "putIfAbsent") #t]
    [(equal? m-call "remove") #t]
    [(equal? m-call "dec") #t]
    [(equal? m-call "pop") #t]
    [(equal? m-call "deq") #t]
    [(equal? m-call "enq") #t]
    [(equal? m-call "push") #t]
    [else #f]))





(define (optimistic-repairable? h instr-list)
  ;; (display "opt repairable? : ") (displayln instr-list)
  (cond
    [(empty? instr-list) #t]
    [(Single-branch? (first instr-list))
     (optimistic-repairable?
      h
      (append
       (Single-branch-branch (first instr-list))
       (rest instr-list)))]
    [(Branch? (first instr-list))
     (and
      (optimistic-repairable?
       h
       (append
        (Branch-branch1 (first instr-list))
        (rest instr-list)))
      (optimistic-repairable?
       h
       (append
        (Branch-branch2 (first instr-list))
        (rest instr-list))))]
    [(and
      (C-Instruction? (first instr-list))
      (equal? (C-Instruction-instr-id (first instr-list))
              (hole-before h)))
     (not (is-writer (Run-method-method (first instr-list))))]

    [(Run-method? (first instr-list))
     (and
      (not (is-writer (Run-method-method (first instr-list))))
      (optimistic-repairable? h (rest instr-list)))]
    [else
     (optimistic-repairable? h (rest instr-list))]
    ))
              


(define (all-configurations hole-list)
  (map
   (lambda (i)
     (cons (take hole-list i) (drop hole-list i)))
   (range (length hole-list))))
     

(define (optimistic-solution h instr-list [crucial-const "1"])
  ;; (displayln solution-contents)
  ;; (displayln "writing optimsitic solution")


  ;; (displayln h)

  (define object
    (cond
      [use-stack "stack"]
      [use-map "map"]
      [else
       "map"]))


  (define feasible-solution
    (reduce (lambda (a b) (and a b))
            (map (lambda (single-h)
                   (optimistic-repairable?
                    single-h
                    instr-list))
                    ;; (Method-instr-list
                    ;;  (get-lib-method library "copySKETCH"))))
                 h)))

  (define optimistic-time-start (current-inexact-milliseconds))
  (define spit-out
    (cond
      [feasible-solution 
       (with-handlers ([(lambda (exn) (equal? exn "no_solution")) (lambda (exn) #f)])
         (optimistic-sketch-from-hole-list result-trace-lists h library object
                                           (Method-args (get-lib-method library "copySKETCH"))
                                           crucial-const))]
      [else
       #f]))



  (cond
    [spit-out
          ;; (optimistic-repairable? (first h) (Method-instr-list
                   ;;                                    (get-lib-method library "copySKETCH"))))
     (system "rm -f sketch-dump.rkt")
     (with-output-to-file "sketch-dump.rkt"
       (lambda () (printf (first spit-out))))
  
     ;; Resolve sketch
     (define throwout (system "~/racket/bin/racket sketch-dump.rkt > OUTPUT.txt"))
     ;; (define throwout2 (system "rm -f sketch-dump.rkt"))
     
     ;; Retrieve the appropriate optimistic conditions
     (define optimistic-conditions
       (with-handlers ([(lambda (exn) #t) (lambda (exn) #f)])
         (string->opts (file->string "OUTPUT.txt"))))
     ;; (define throwout3 (system "rm -f OUTPUT.txt"))
     
     
     (displayln optimistic-conditions)
     (displayln "-----------------------")

     (cond
       [optimistic-conditions
        ;; Use the resolved sketch optimistic conditions to repair the library extension
        (define opt-answer
          (optimistic-repair
           (second spit-out) ;; All of the traces, with meta information
           optimistic-conditions ;; Results from the sketch
           library
           "copySKETCH"))

        (define optimistic-time-end (current-inexact-milliseconds))
        (display "Optimistic repair time: ") (displayln (- optimistic-time-end optimistic-time-start))
        
        opt-answer]
       [(equal? crucial-const "1")
        (optimistic-solution h instr-list "2")]

       [else
        #f])
     
     
     ]
     
    [else
     #f]))
  

(define (no-conflicts h lock-h-list)
  (empty?
   (filter
    (lambda (lock-h)
      (>= (hole-before h) (hole-before lock-h))
      )
    lock-h-list)))
     

(define (all-compatible-holes opt-h-list lock-h-list)

  (reduce
   (lambda (a b) (and a b))
   (map
    (lambda (h) (no-conflicts h lock-h-list))
    opt-h-list)))




(define (all-solutions-for-hole-list h-list)
  (define all-splits (all-configurations h-list))
  (define fixed-splits
    (filter (lambda (s) (and
                         (not (empty? (car s)))
                         (not (empty? (cdr s)))))
            all-splits))


  (define (all-repairable h-list instr-list)

    (reduce (lambda (a b) (and a b))
            (map (lambda (single-h)
                   (optimistic-repairable?
                    single-h 
                    instr-list))
                 h-list)))




  (displayln     (filter (lambda (s) (and
                         (not (empty? (car s)))
                         (not (empty? (cdr s)))))
                    all-splits))

  (append
   (list (optimistic-solution h-list
                              (simplify-redundant-method
                               (Method-instr-list
                                (get-lib-method library "copySKETCH")))))
   (list 
    (locked-solution h-list
                     (simplify-redundant-method
                      (Method-instr-list (get-lib-method library "copySKETCH")))))
   (list (announcement-solution h-list))
   (map
    
    (lambda (s)
      (display "optimistic side: ") (displayln (car s))
      (display "locked side: ") (displayln (cdr s))
      
      ;; (if (all-repairable (car s) (Method-instr-list (get-lib-method library "copySKETCH")))

      (if (all-compatible-holes (car s) (cdr s))
          (begin
            (displayln "all compatible holes:")
            (displayln (car s))
            (displayln (cdr s))
            (displayln (all-compatible-holes (car s) (cdr s)))

           (let
              ([opt-sol (optimistic-solution (car s)
                                             (simplify-redundant-method
                                              (Method-instr-list (get-lib-method library "copySKETCH")))
                                             )])
            (if opt-sol
                (locked-solution (cdr s) opt-sol)
                #f)))
      ;; #f))

      #f))
      fixed-splits)))


;; (displayln filtered-hole-set)
    
;; (define solution-time-start (current-inexact-milliseconds))
(define solution-set
  (reduce
   append
   (map
    (lambda (h)

      (all-solutions-for-hole-list h))
      ;; (displayln h)


      ;; (append

      ;;  (list (locked-solution h))
      ;;  (list (announcement-solution h))
      ;;  (list (cons (optimistic-solution h) `()))
      ;;  ))

    filtered-hole-set)))

;; (define solution-time-end (current-inexact-milliseconds))
;; (define solution-time (- solution-time-end solution-time-start))



(displayln (method->c (get-lib-method library "copySKETCH")))

(map  (lambda (s)
        (cond
          [(not s)
            #f]
          [(list? (car s))
           (displayln (method->c (Method "copySKETCH" (list "Map" "Map" "int") "int" (car s))))]
          [else
           (displayln (method->c (Method "copySKETCH" (list "Map" "Map" "int") "int" s)))]))
             ;; s)))


              solution-set)



;; (let*
;;     ;; ([lock-set
;;     ;;   (cdr (first solution-set))])
;;       ;; (None)])
    
;;   ;; (displayln (method->c (Method "copySKETCH" (list "List" "List" "int")  "int" (car (second solution-set)))
;;   ;;                       "lock"))
;;     ;; (displayln (method->c (Method "copySKETCH" (list "Shared_int")  "int" (car (first solution-set)
;;     ;;                                                                                 ))
;;     ;;                       "lock"))

;;     ;; (displayln (method->c (Method "copySKETCH" (list "Shared_int")  "int" (fourth solution-set)
;;     ;;                                                                                 )
;;   ;;                       "lock"))
;;   (void)

;;                         ;; lock-set))

;;   ;; (displayln (method->c (Method "copySKETCH" (list "Shared_int")  "int" (car (first solution-set)
;;   ;;                                                                                   ))
;;   ;;                       "local"
;;   ;;                       lock-set))

;;   ;; (displayln lock-set))
;;   )

(display "seq-time: ")(displayln seq-time)
(display "error-time: ")(displayln error-time)
(display "error-trace-num: ") (displayln error-trace-num)
(display "num-hole-sets: ") (displayln num-hole-sets)

(display "OPTIMISTIC REPAIRABLE? ")
(displayln  (optimistic-repairable? (first (first filtered-hole-set)) (Method-instr-list (get-lib-method library "copySKETCH"))))


;; (displayln solution-time)
;; (define merged-set
;;   (reduce
;;    append
;;    (map
;;     (lambda (h)
;;       (displayln (connect-solutions-end-to-end (second solution-set)
;;                                                (locked-solution  (rest h))
;;                                                (first h)
;;                                                (second h)
;;                                                1
;;                                                0)))
;;     filtered-hole-set)))







