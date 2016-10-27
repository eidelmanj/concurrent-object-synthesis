#lang racket
(require "../program_representation/simulator-structures.rkt")
(require "../utilities/utilities.rkt")

(provide
 string->opts
 (struct-out Method-Call-Short)
 (struct-out Argument-Ref)
 (struct-out Variable)
 (struct-out Opt-Inequality)

 )


(define test-str 
"5
Trace: pick-trace
running method: contains
result: #t
running method: get
result: 5
running method: remove
result: 5
running method: remove
result: #<None>
possible: #t
TRACE-TYPE: no-optimistic-restart
Trace: pick-trace
running method: contains
result: #t
running method: get
result: 5
running method: push
result: 0
running method: remove
result: 0
possible: #t
TRACE-TYPE: no-optimistic-restart
/u/eidelmanj/research/concurrent-object-synthesis/examples/verification-dump.rkt:255:0
'(define method-choice12
   (lambda ()
     (METHOD-get (list (list-ref first-args 0) (list-ref first-args 1)))))
/u/eidelmanj/research/concurrent-object-synthesis/examples/verification-dump.rkt:267:0
'(define var-choice12 (lambda () val))
/u/eidelmanj/research/concurrent-object-synthesis/examples/verification-dump.rkt:279:0
'(define method-choice11
   (lambda ()
     (METHOD-get (list (list-ref first-args 0) (list-ref first-args 1)))))
/u/eidelmanj/research/concurrent-object-synthesis/examples/verification-dump.rkt:291:0
'(define var-choice11 (lambda () val))
/u/eidelmanj/research/concurrent-object-synthesis/examples/verification-dump.rkt:303:0
'(define OPT1
   (lambda ()
     (&&
      (not (equal? (method-choice11) (var-choice11)))
      (not (equal? (method-choice12) (var-choice12))))))
")



(define (filter-str str)
  
  (let
      ([all-lines (string-split str "\n")])
    (let
        ([only-racket-lines
          (filter
           (lambda (l) ;; (displayln (string-ref l 0))
                   (or
                    (equal? (string (string-ref l 0)) "(")
                    (equal? (string (string-ref l 0)) " ")
                    (equal? (string (string-ref l 0)) "'")))
           all-lines)])


      (let
          ([back-together (reduce
                           (lambda (a b) (string-append a "\n" b))
                           only-racket-lines)])

        (let
            ([filtered-str
              (string-replace back-together "'(define " "")])
          (let
              ([filtered-str2
                (string-replace filtered-str "(lambda ()" "\n")])

            filtered-str2))))))


(define (is-var-keyword s)
  (and
   (> (string-length (string-replace s " " "")) 3)
   (equal? (substring (string-replace s " " "") 0 3) "var")))

(define (is-keyword s)
  (or
      (and
       (> (string-length (string-replace s " " "")) 6)
       (equal? (substring (string-replace s " " "") 0 6) "method"))
      (and
       (> (string-length (string-replace s " " "")) 3)
       (equal? (substring (string-replace s " " "") 0 3) "var"))))
      ;; (and
      ;;  (> (string-length s) 3)
      ;;  (equal? (substring s 0 3) "OPT"))))

(define (is-opt-keyword s)
  (and
   (> (string-length (string-replace s " " "")) 3)
   (equal? (substring (string-replace s " " "") 0 3) "OPT")))





(define (until-next-keyword l)
  (cond
    [(empty? l) `()]
    [(is-keyword (first l))
     ;; (display "found keyword: ") (displayln (first l))
     `()]
    [(is-opt-keyword (first l)) `()]
    [else
     (append (list (first l)) (until-next-keyword (rest l)))]))

(define (after-next-keyword l)
  (cond
    [(empty? l) `()]
    [(is-keyword (first l))
      l]
    [(is-opt-keyword (first l)) l]
    [else
     (after-next-keyword (rest l))]))
  
(struct Method-Call-Short (name args) #:transparent)
(struct Argument-Ref (id) #:transparent)
(struct Variable (id) #:transparent)
(struct Opt-Inequality (elem1 elem2) #:transparent)


(define (create-arg-objects str)
  (let*
      ([arg-list (string-split str "(")]
       [arg-list-sanitized (map (lambda (s) (string-replace s ")" ""))
                                arg-list)]
       [arg-objects
        (map
         (lambda (s)
           (cond
             [(regexp-match-positions #rx"list-ref" s)
              (Get-argument (string->number (third (string-split s " "))))]
             [else
              (Variable s)]))
         arg-list-sanitized)]
       )
    arg-objects))


;; Note: This assumes there is an even number of items.
;; must be true because all of these are inequalities between two elements
(define (collect-all-parts l)
  (cond
    [(empty? l) `()]
    [(equal? (length l) 1)
     (displayln "This error should never happen")
     `()]
    [else
     (append
      (list (Opt-Inequality (first l) (second l)))
      (collect-all-parts (rest (rest l))))]))
     


(define (str-list-to-map str-list keyword-map opt-map)
  (cond
    [(empty? str-list) (void)]

    [(is-opt-keyword (first str-list))
     (let*
         ([str-version
           (reduce string-append (until-next-keyword (rest str-list)))]
          [filtered-str-version
           (string-replace
            str-version
            "not (equal?"
            "")]
          [all-parts

           (map
            (lambda (s)
              (substring
               s
               (if (regexp-match-positions #rx"method" s)
                   (car (first (regexp-match-positions #rx"method" s)))
                   (car (first (regexp-match-positions #rx"var" s))))
               (string-length s)))

            (filter
             (lambda (s)
               (or
                (regexp-match-positions #rx"method" s)
                (regexp-match-positions #rx"var" s)))
             (string-split (substring filtered-str-version (+
                                                            (cdr (first
                                                                  (regexp-match-positions #rx"\\(" filtered-str-version)))
                                                            3)
                                      (string-length filtered-str-version)) ")")))]

          [opt-id
           (substring (first str-list) 3 (string-length (first str-list)))]



          )

       (hash-set! opt-map
                  (string->number opt-id)
                  (collect-all-parts all-parts))
       
       (str-list-to-map (after-next-keyword  (rest str-list)) keyword-map opt-map)
       )]

    
    [(is-var-keyword (first str-list))
     (hash-set! keyword-map (string-replace (first str-list) " " "")
                (string-replace
                 (string-replace
                  (reduce string-append (until-next-keyword (rest str-list)))
                  ")"
                  "")
                 " "
                 ""))
     ;; (displayln (after-next-keyword (rest str-list)))
     (str-list-to-map (after-next-keyword  (rest str-list)) keyword-map opt-map)]
    [(is-keyword (first str-list))
     (let*
         ([str-version 
           (reduce string-append (until-next-keyword (rest str-list)))]
          [find-method (first (regexp-match-positions #rx"METHOD-" str-version))]
          [find-args (first (regexp-match-positions #rx"list " str-version))]
          [from-method-name
           (substring str-version (cdr find-method) (string-length str-version))]
          [from-method-args (substring str-version (cdr find-args) (string-length str-version))]
          [arg-objects (create-arg-objects from-method-args)]
          [method-name (first (string-split from-method-name))])

       ;; (display "found method: ") (displayln method-name)
       ;; (display "with args: ") (displayln arg-objects)
       
       ;; (hash-set! keyword-map (string-replace (first str-list) " " "")
       ;;            str-version)

       (hash-set! keyword-map (string-replace (first str-list) " " "")
                    (Method-Call-Short method-name arg-objects))
         ;; (displayln (after-next-keyword (rest str-list)))
         (str-list-to-map (after-next-keyword  (rest str-list)) keyword-map opt-map))]
    [else
     (str-list-to-map (after-next-keyword (rest str-list)) keyword-map opt-map)]))
     


(define (parse-str str)
  
  (let
      ([str-list (string-split (filter-str str) "\n")])

    (define keyword-map (make-hash))
    (define opt-map (make-hash))
    (str-list-to-map str-list keyword-map opt-map)
    (cons keyword-map opt-map)
    ))





(define (substitute-arguments opt-map keyword-map)
  (map

   (lambda (key)
     (let*
         ([all-ineqs (hash-ref opt-map key)]
          [substitutions

           (map
            (lambda (ineq)
              (Opt-Inequality
               (hash-ref keyword-map (Opt-Inequality-elem1 ineq))
               (hash-ref keyword-map (Opt-Inequality-elem2 ineq))))
               
            all-ineqs)])
       (hash-set! opt-map key substitutions)))
   

   (hash-keys opt-map)))
   


(define (string->opts str)
  

  (define ret-pair (parse-str str))
  (define opt-map (cdr ret-pair))
  (define keyword-map (car ret-pair))



  (substitute-arguments opt-map keyword-map)
  (displayln opt-map)
  opt-map)




