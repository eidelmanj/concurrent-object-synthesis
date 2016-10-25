#lang racket

(require
 "../program_representation/simulator-structures.rkt"
 "../utilities/utilities.rkt"
 "../internal_libs/map-library.rkt")



(provide method->c)

(define (expr->c e)
  (match e
    [(Dereference id type offset)
     (string-append id "->" offset)]
    [(Equal e1 e2)
     (string-append "("(expr->c e1)") == (" (expr->c e2) ")")]
    [(Not-equal e1 e2)
     (string-append "("(expr->c e1)") != (" (expr->c e2) ")")]
    [(Not e)
     (string-append "!(" (expr->c e) ")")]
    [(And e1 e2)
     (string-append "("(expr->c e1)") && (" (expr->c e2) ")")]
    [(Or e1 e2)
     (string-append "("(expr->c e1)") || (" (expr->c e2) ")")]
    [(Is-none? e)
     (string-append (expr->c e) " == NONE_VAL")]
    [(Get-var v)
     v]
    [(New-struct type arg-list)
     (string-append "NEW_" type "();")]
    [(Get-argument id)
     (string-append "a" (~v id))]


      
    [_
     (~v e)]))

(define (instr-list->c instr-list)
  (cond
    [(empty? instr-list)
     ""]
    [else
     (string-append
      (match (first instr-list)
        [(Single-branch c b)
         (string-append
          "if (" (expr->c c) ") {\n"
          (instr-list->c b)
          "}\n")]

        [(Loop c i)
         (string-append
          "while (" (expr->c c) ") {\n"
          (instr-list->c i)
          "}\n")]

        [(Create-var id type)
         (string-append type " " id ";\n")]
        [(Set-var id assignment)
         (string-append id " = " (expr->c assignment) ";\n")]
        [(Run-method m args ret)
         (string-append
          ret " = " m "("
          ;; include all args
          (reduce
           string-append
           (map
            (lambda (a)
              (string-append (expr->c a) ", "))
            args))
          
          ");\n")]

        [(Set-pointer id tp offset assignment)
         (string-append
          id "->" offset " = " (expr->c assignment) ";\n")]

        [(Return val)
         (string-append "return " (expr->c val) ";\n")]

        [(Lock id)

         (string-append "pthread_mutex_lock(l" (~v id) ");\n")]
        [(Unlock id)
         (string-append "pthread_mutex_unlock(l" (~v id) ");\n")]


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
      (instr-list->c (rest instr-list)))]))
     
             
                   
         


(define (method->c method)
  (define count (void))
  (set! count 0)
  (let*
      ([instr-list (Method-instr-list method)]
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

    (instr-list->c instr-list)
    "}\n")))



     
  
(displayln (method->c (first map-library)))
