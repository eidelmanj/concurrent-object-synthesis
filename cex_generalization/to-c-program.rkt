#lang racket

(require
 "../program_representation/simulator-structures.rkt"
 "../utilities/utilities.rkt"
 "../internal_libs/map-library.rkt")



(provide method->c)


(define (primitive-type e types)
  (match e
    [(Get-var v)
     (equal? (hash-ref types v) "int")]
    [_ (displayln "ERROR: Tried to get type of invalid var")]))


(define (pointer-type e types)
  (not (primitive-type e types)))


(define (expr->c e types)
  (match e
    [(Dereference id type offset)
     (string-append id "->" offset)]
    [(Equal e1 e2)
     (string-append "("(expr->c e1 types)") == (" (expr->c e2 types) ")")]
    [(Not-equal e1 e2)
     (string-append "("(expr->c e1 types)") != (" (expr->c e2 types) ")")]
    [(Not e)
     (string-append "!(" (expr->c e types) ")")]
    [(And e1 e2)
     (string-append "("(expr->c e1 types)") && (" (expr->c e2 types) ")")]
    [(Or e1 e2)
     (string-append "("(expr->c e1 types)") || (" (expr->c e2 types) ")")]
    [(Is-none? e)
     (if (primitive-type e types)
         (string-append (expr->c e types) " == NONE_VAL")
         (string-append (expr->c e types) " == NULL"))]
    [(Get-var v)
     v]
    [(New-struct type arg-list)
     (string-append "NEW_" type "();")]
    [(Get-argument id)
     (string-append "a" (~v id))]


      
    [_
     (~v e)]))


(define (find-matching-lock hole-list id)
   (define matching-holes
     (filter (lambda (h) (displayln h) (displayln id)(equal? id (cdr h)))
          hole-list))

   (cond
     [(empty? matching-holes)
      (displayln "ERROR: Tried to find lock that doesn't exist")
      `()]
     [else
      (car (first matching-holes))]))



(define (instr-list->c instr-list types [mode  "lock"] [lock-set `()])
  ;; (display "Inst-list->c with lock set: ") (displayln lock-set)
  (cond
    [(empty? instr-list)
     ""]
    [else
     (string-append
      (match (first instr-list)
        [(Single-branch c b)
         (string-append
          "if (" (expr->c c types) ") {\n"
          (instr-list->c b types mode lock-set)
          "}\n")]

        [(Loop c i)
         (string-append
          "while (" (expr->c c types) ") {\n"
          (instr-list->c i types mode lock-set)
          "}\n")]

        [(Branch c b1 b2)
         (string-append
          "if (" (expr->c c types) ") {\n"
          (instr-list->c b1 types mode lock-set)
          "} else {\n"
          (instr-list->c b2 types mode lock-set)
          "}\n")]

        [(Create-var id type)
         (hash-set! types id type)
         (string-append type " " id ";\n")]
        [(Set-var id assignment)
         (string-append id " = " (expr->c assignment types) ";\n")]
        [(Run-method m args ret)
         (let*
             ([arg-str
               (reduce
                string-append
                (map
                 (lambda (a)
                   (string-append (expr->c a types) ", "))
                 args))])

           (string-append
            ret " = " m "("
            ;; include all args
            

            (substring arg-str
                       0
                       (- (string-length arg-str) 2))

            ");\n"))]

        [(Set-pointer id tp offset assignment)
         (string-append
          id "->" offset " = " (expr->c assignment types) ";\n")]

        [(Return val)
         (string-append "return " (expr->c val types) ";\n")]

        [(Lock id)
         (match mode
           ["lock"
            (string-append "pthread_mutex_lock(l" (~v id) ");\n")]
           ["RW"
            (string-append "pthread_rwlock_wrlock(l" (~v id) ");\n")]
           ["local"
            (let*
                ([hole (find-matching-lock lock-set id)]
                 [interruptor (Hole-interruptor hole)]
                 [interrupt-args (Run-method-args interruptor)])


              (string-append
               "pthread_rw_lock *l" (~v id) ";\n"
               "l" (~v id) " = GET_LOCK(" (expr->c (first interrupt-args) types) ", " (expr->c (second interrupt-args) types)
               ");\n"

               "pthread_mutex_lock(l" (~v id) ");\n"

               ))])]

        [(Unlock id)
         (match mode 
           ["lock" 
            (string-append "pthread_mutex_unlock(l" (~v id) ");\n")]
           ["RW"
            (string-append "pthread_rwlock_unlock(l" (~v id) ");\n")]
           ["local"
            
            (string-append "pthread_mutex_unlock(l" (~v id) ");\n")])]
        


        [(Label id)
         (string-append "label LABEL" (~v id) ":\n")]
        [(Goto id c)
         (string-append "goto LABEL" (~v id)";\n")]

        ;; [(Argument-ref id)
        ;;  (string-append "a" id)]
        [(None)
         
         (string-append "NONE_VAL")]
        
        [_
         (string-append "TODO:" (~v (first instr-list)))])
      (instr-list->c (rest instr-list) types mode lock-set))]))
     
             
                   
         


(define (method->c method [mode "RW"] [lock-set `()])
  (define count (void))
  (set! count 0)
  (let*
      ([instr-list (Method-instr-list method) ]
       [nm (Method-id method)]
       [ret-type (Method-ret-type method)]
       [arg-types (Method-args method)])

    ;; Method declaration
    (string-append
     ret-type " " nm "("

     ;; Include all arguments
     (reduce
      string-append
      (map
       (lambda (r)
         (let
             ([arg-str
               (string-append r " a" (~v count) ", ")])
           (set! count (+ 1 count))
           arg-str))
       arg-types))

    ") {\n"

    (instr-list->c instr-list (make-hash) mode lock-set)
    "}\n")))



     
  
(displayln (method->c (first map-library)))
