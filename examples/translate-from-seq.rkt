#lang racket
(require "../program_representation/simulator-structures.rkt")
(require "../utilities/utilities.rkt")
(require "../examples/mooly-example.rkt")
(require "../examples/mooly-library.rkt")

(require racket/string)

(require "../cex_generalization/to-sketch.rkt")
;; (require "../cex_generalization/read-back-answer.rkt")
(require "../cex_generalization/metasketcher.rkt")

(require "../error_trace_generation/error-trace.rkt")
(require (only-in "../error_trace_generation/linearizable.rkt" lin-result-trace))
(require (only-in "../error_trace_generation/methods.rkt" number-lines))
(require "../optimal_cover/cover.rkt" )


(require "../parser/parser.rkt")
(require         "../program_representation/racket-synthesis.rkt")


(define input-string "

void copySKETCH(List* l1, List* l2, int z0) {
  _out = 0;
  int  z3_s111=0;
  remove(l1, z0, z3_s111);
  add(l2, z3_s111);
  _out = z3_s111;
  return;

}

")


  ;; _out = 0;
  ;; int  z3_s111=0;
  ;; remove(l1, z0, z3_s111);
  ;; add(l2, z3_s111);
  ;; _out = z3_s111;
  ;; return;



(define lst (let*
                ((test-program input-string)
                 (input (open-input-string test-program)))
              (translate (simple-math-parser (lex-this simple-math-lexer input)))))


(define (transform-arguments expr arg-list)
  expr)

(define (all-but-last l)
  (cond
    [(empty? l) empty]
    [(empty? (rest l)) empty]
    [else
     (append
      (list (first l))
      (all-but-last (rest l)))]))


(define (get-var-id entry)
  (match (Get-var-id entry)
    [(var-exp a) a]
    [else (Get-var-id entry)]))


(define (find-argument-match entry arg-list)

  (define (find-argument-helper arg-list i)
    ;; (display "arg-list: ")(displayln arg-list)
    ;; (display "entry: ") (displayln entry)
    (cond
      [(empty? arg-list)
       null]
      [else

       (let*
           ([arg  (Argument-id (first arg-list))]
            [entry-val (get-var-id entry)])
         (cond
           [(equal? arg entry-val)
            i]
           [else
            (find-argument-helper (rest arg-list) (+ i 1))]))]))

  (find-argument-helper arg-list 0))
         

(define (reference-arg-list e arg-list)
  (let*
      ([matching-entry-number (find-argument-match e arg-list)])

  (cond
    [(null? matching-entry-number)
     (Get-var (get-var-id e))]
    [else
     (Get-argument matching-entry-number)])))


;; (define (expression-create-arg-refs e arg-list)
;;   (match e
;;     [(Get-var a) (reference-arg-list a arg-list)]
;;     [(Dereference id tp offset) (Dereference (reference-arg-list id arg-list) tp offset)]
;;     [(
         

(define (translate-arg-list method-args arg-list)


  
  (map
   (lambda (entry)

     (let*
         ([matching-entry-number (find-argument-match entry  arg-list)]
          )

       (cond
         [(null? matching-entry-number)
          (Get-var (get-var-id entry))]
         [else
          (Get-argument matching-entry-number)])))
          


   (all-but-last (Arguments-arg-list method-args))))

(define (get-return-var method-args)
  (match
      (Get-var-id (last (Arguments-arg-list method-args)))
    [(var-exp i) i]
    [else
     (Get-var-id (last (Arguments-arg-list method-args)))]))




(define (remove-uneccessary-get-var a)
  ;; (display "REMOVE UNECCESSARY: ")
  ;; (displayln a)
  ;; (if (string? a) (displayln "STRING") (displayln "nope"))
    
  (match a
    [(Get-var b) b]
    [(var-exp e) (remove-uneccessary-get-var e)]
    [_ a]))



(define (argument-definitions arg-list)
  (reduce
   append
   (map
    (lambda (a)
      (list
       (Create-var (Argument-id a) (Argument-type a))
       (Set-var (Argument-id a) (Get-argument (find-argument-match (Get-var (Argument-id a)) arg-list)))))
    arg-list)))



(define (get-type-from-arg-list id arg-list defined-list)
  ;; (display "id: ") (displayln id)
  ;; (display "arg-list: ") (displayln arg-list)
  ;; (display "defined-list: ") (displayln defined-list)
  
  (let*
      ([matching-args (filter
                  (lambda (a) (equal? (Argument-id a) (remove-uneccessary-get-var id)))
                  arg-list)]
       [matching-def (filter
                  (lambda (a) (equal? (Argument-id a) (remove-uneccessary-get-var id)))
                  defined-list)]
       [matching (append matching-args matching-def)])
    (cond
      [(empty? matching)
       (display "ERROR: Did not find referenced id - ") (displayln id)
       (exit)]
      [else
       (Argument-type (first matching))])))
    

(define (repair-expression e arg-list defined-list)
  (match e
    [(Set-pointer id tp offset val)

     (Set-pointer
      (remove-uneccessary-get-var id)
      (get-type-from-arg-list  id arg-list defined-list)

      (remove-uneccessary-get-var offset)
      (repair-expression val arg-list defined-list))]
    [(Create-var id tp)
     (Create-var
      (remove-uneccessary-get-var id)
      (remove-uneccessary-get-var tp))]
    [(Set-var id assignment)
     (Set-var
      (remove-uneccessary-get-var id)
      (repair-expression assignment arg-list defined-list))]


    [(Dereference id tp offset)
     ;; (match offset
     ;;   [(Dereference id2 tp2 offset2)
     ;;    (repair-expression (Set-var "TMP-VAR" id tp id2) arg-list defined-list)
     ;;    (repair-expression (S
     ;;   [_
        (Dereference
         (remove-uneccessary-get-var id)
         (match tp
           [null (get-type-from-arg-list  (remove-uneccessary-get-var id) arg-list defined-list)]
           [else (remove-uneccessary-get-var tp)])
         

         (remove-uneccessary-get-var offset))]

    [(Equal e1 e2)
     (cond
       [(None? e1) (Is-none? (repair-expression e2 arg-list defined-list))]
       [(None? e2) (Is-none? (repair-expression e1 arg-list defined-list))]
       [else
        (Equal (repair-expression e1 arg-list defined-list) (repair-expression e2 arg-list defined-list))])]
         
    [(Equal e1 e2)
     (Equal (repair-expression e1 arg-list defined-list) (repair-expression e2 arg-list defined-list))]
    [(Not-equal e1 e2)
     (Not-equal (repair-expression e1 arg-list defined-list) (repair-expression e2 arg-list defined-list))]
    [(Not e1)
     (Not (repair-expression e1 arg-list defined-list))]
    [(Or e1 e2)
     (Or (repair-expression e1 arg-list defined-list) (repair-expression e2 arg-list defined-list))]
    [(And e1 e2)
     (And (repair-expression e1 arg-list defined-list) (repair-expression e2 arg-list defined-list))]
    [(Add e1 e2)
     (Add (repair-expression e1 arg-list defined-list) (repair-expression e2 arg-list defined-list))]
    [(Subtract e1 e2)
     (Subtract (repair-expression e1 arg-list defined-list) (repair-expression e2 arg-list defined-list))]
    [(Divide e1 e2)
     (Divide (repair-expression e1 arg-list defined-list) (repair-expression e2 arg-list defined-list))]
    [(Multiply e1 e2)
     (Multiply (repair-expression e1 arg-list defined-list) (repair-expression e2 arg-list defined-list))]
    [(Less-than e1 e2)
     (Less-than (repair-expression e1 arg-list defined-list) (repair-expression e2 arg-list defined-list))]
    [(Less-than-equal e1 e2)
     (Less-than-equal (repair-expression e1 arg-list defined-list) (repair-expression e2 arg-list defined-list))]
    [(Greater-than e1 e2)
     (Greater-than-equal (repair-expression e1 arg-list defined-list) (repair-expression e2 arg-list defined-list))]
    [(Is-none? e1)
     (Is-none? (repair-expression e1 arg-list defined-list))]
    [_  e]))



(define (complete-racket-translation instr-list arg-list defined-list)
  ;; (displayln "translating line...")
  (cond
    [(empty? instr-list) `()]



    [(Branch? (first instr-list))
     (append
      (list 
       (Branch (repair-expression (Branch-condition (first instr-list)) arg-list defined-list)
               (complete-racket-translation (Branch-branch1 (first instr-list)) arg-list defined-list)
               (complete-racket-translation (Branch-branch2 (first instr-list)) arg-list defined-list)))
      (complete-racket-translation (rest instr-list) arg-list defined-list))]

    [(Loop? (first instr-list))
     ;; (display "Loop: ") (displayln defined-list)
     (append
      (list
       (Loop (repair-expression (Loop-condition (first instr-list)) arg-list defined-list)
             (complete-racket-translation (Loop-instr-list (first instr-list)) arg-list defined-list)))
      (complete-racket-translation (rest instr-list) arg-list defined-list))]

    [(Single-branch? (first instr-list))
     (append
      (list
       (Single-branch (repair-expression (Single-branch-condition (first instr-list)) arg-list defined-list)
                      (complete-racket-translation (Single-branch-branch (first instr-list)) arg-list defined-list)))
       (complete-racket-translation (rest instr-list) arg-list defined-list))]

    [(and
      (Run-method? (first instr-list))
      (equal?
       (substring (Run-method-method (first instr-list)) 0 3)
       "new")
      )

     ;; (display "found inititializer")
     ;; (displayln (first instr-list))





     (let*
         ([defn-list (append
                      (list (Argument
                             (substring (Run-method-method (first instr-list))
                                        3
                                        (string-length
                                         (Run-method-method (first instr-list))))
                             (get-return-var (Run-method-args (first instr-list)))))
                      defined-list)])
     


     (append
      (list
       (Create-var (get-return-var (Run-method-args (first instr-list)))
                   (substring (Run-method-method (first instr-list)) 3 (string-length
                                                                        (Run-method-method (first instr-list)))))
       (Set-var (get-return-var (Run-method-args (first instr-list)))
                (New-struct (substring (Run-method-method (first instr-list))
                                       3
                                       (string-length
                                        (Run-method-method (first instr-list))))
                            (map
                             get-var-id
                             (Arguments-arg-list (Run-method-args (first instr-list)) )))))
      (complete-racket-translation (rest instr-list) arg-list defn-list)))]

    
    [(Set-var? (first instr-list))
     (append
      (list
       (Set-var (remove-uneccessary-get-var (Set-var-id (first instr-list))) (repair-expression (Set-var-assignment (first instr-list)) arg-list defined-list )))
      (complete-racket-translation (rest instr-list) arg-list defined-list))]
    [(and
      (Create-var? (first instr-list))
      (Set-var? (Create-var-id (first instr-list))))
     (append
      (list
       (Create-var (Set-var-id (Create-var-id (first instr-list))) (Create-var-type (first instr-list)))
       (repair-expression (Create-var-id (first instr-list)) arg-list defined-list))
        
      (complete-racket-translation (rest instr-list) arg-list
                                   (append
                                    (list (Argument (Create-var-type (first instr-list))
                                                    (Set-var-id (Create-var-id (first instr-list)))))
                                   defined-list)))]



    [(Run-method? (first instr-list))
     (append
      (list
       (Run-method
        (Run-method-method (first instr-list))
        (translate-arg-list  (Run-method-args (first instr-list)) arg-list )
        (get-return-var (Run-method-args (first instr-list)))))
      (complete-racket-translation (rest instr-list) arg-list defined-list))]

    [(Return? (first instr-list))
     (append
      (list
       (Return (Get-var "_out")))
      (complete-racket-translation (rest instr-list) arg-list defined-list))]

    [(and
      (Set-pointer? (first instr-list))
      (Dereference? (Set-pointer-offset (first instr-list))))

     (let*
         ([deref (Set-pointer-offset (first instr-list))])

       (append
        (list

         (Set-var "TMP-PTR"

                   (Dereference
                    (remove-uneccessary-get-var (Set-pointer-id (first instr-list)));; (remove-uneccessary-get-var (Dereference-id deref))
                    
                    "List"
                    (remove-uneccessary-get-var (Dereference-id deref))));; (remove-uneccessary-get-var (Dereference-offset deref))))

         (Set-pointer
          "TMP-PTR"
          ;; (get-type-from-arg-list (Set-pointer-id (first instr-list)) arg-list defined-list)
          ;; (Dereference-offset deref)
          "Node"
          (remove-uneccessary-get-var (Dereference-offset deref))
          (repair-expression (Set-pointer-val (first instr-list)) arg-list defined-list)))
        (complete-racket-translation (rest instr-list) arg-list defined-list)))]
    
    [(Set-pointer? (first instr-list))
     (append
      ;; (list
      ;;  (Set-pointer
      ;;   (remove-uneccessary-get-var (Set-pointer-id (first instr-list)))
      ;;   (remove-uneccessary-get-var (Set-pointer-type (first instr-list)))
      ;;   (remove-uneccessary-get-var (Set-pointer-offset (first instr-list)))
      ;;   (remove-uneccessary-get-var (Set-pointer-val (first instr-list)))))
      (list (repair-expression (first instr-list) arg-list defined-list))
      (complete-racket-translation (rest instr-list) arg-list defined-list))]

    [else
     (append
      (list (first instr-list))
      (complete-racket-translation (rest instr-list) arg-list defined-list))]
    ))


      



  

(define (convert-method-args args)
  (map
   (lambda (a)
     (Argument-type a))
   args))








(define extension-method
  (Method
   (Method-id (first lst))


  (convert-method-args (Method-args (first lst)))
  (complete-racket-translation (Method-instr-list (first lst)) (Method-args (first lst)) `())
  (Argument-type (last (Method-args (first lst))))))






;;;; Now lets try with the library itself




(define lib-str   "

void add(List* l, int newVal, int _out) {

  newNode(newN_s186);
  newN_s186->val = newVal;
  if ((l->first) == (NULL)) {
    l->first = newN_s186;
    l->last = newN_s186;
  } else {
    l->last->next = newN_s186;
    l->last = newN_s186;
  }
  _out = newVal;
return;
}


void get(List* l, int idx, int _out) {

  _out = 0;
  Node*  cur=l->first;
  int  i=0;
  while ((cur) != (NULL)) {
    if ((i) == (idx)) {
      _out = cur->val;
      return;

    }
    cur = cur->next;
    i = i + 1;

  }
  _out = -100;
  return;
}


void remove(List* l, int idx, int _out) {
  _out = 0;
  Node*  cur=l->first;
  Node*  toRemove=NULL;
  Node* tmp=NULL;
  int i = 0;
  while ((cur) != (NULL)) {
    if (i == ((idx - 1))) {
      toRemove = cur->next;
      tmp = cur->next;
      cur->next = tmp->next;
      if ((l->last) == (toRemove)) {
        l->last = cur->next;
      }
      _out = toRemove->val;
      return;
    }
    i = i + 1;
   
  }
  _out = -100;
  return;
}



")




  ;; newNode(newN_s186);
  ;; newN_s186->val = newVal;
  ;; if ((l->first) == (NULL)) {
  ;;   l->first = newN_s186;
  ;;   l->last = newN_s186;
  ;; } else {
  ;;   l->last->next = newN_s186;
  ;;   l->last = newN_s186;
;; }



  ;; _out = 0;
  ;; Node*  cur=l->first;
  ;; int  i=0;
  ;; while ((cur) != (NULL)) {
  ;;   if ((i) == (idx)) {
  ;;     _out = cur->val;
  ;;     return;

  ;;   }
  ;;   cur = cur->next;
  ;;   i = i + 1;

  ;; }
  ;; _out = -100;
  ;; return;



(define lst2 (let*
                ((test-program lib-str)
                 (input (open-input-string test-program)))
              (translate (simple-math-parser (lex-this simple-math-lexer input)))))













;; (displayln (Method-id (first lst2)))



;; (argument-definitions (Method-args (first lst2)))


(define (combine-all-repairs instr-list arg-list)
  (append
   (argument-definitions arg-list)
   (complete-racket-translation instr-list arg-list `())))


;; (map
;;  (lambda (l)
;;    (displayln l))

(define method-choice (third lst2))
(define repaired-instr-list 
  (combine-all-repairs (Method-instr-list method-choice) (Method-args method-choice)))

(define (grab-method-args-types arg-list)
  (map
   (lambda (a) (Argument-type a))
   (all-but-last arg-list)))


(define (repair-all-methods library)
  (map
   (lambda (m)
     (Method
      (Method-id m)
      (grab-method-args-types (Method-args m))
      (Method-ret-type m)
      (combine-all-repairs (Method-instr-list m) (Method-args m))))
   library))

(define test-lib
  (list
   (Method
    (Method-id method-choice)
    (grab-method-args-types (Method-args method-choice))
    (Method-ret-type method-choice)
    repaired-instr-list)))


(displayln (repair-all-methods lst2))


;; (map
;;  displayln
;;  (Method-instr-list method-choice))

;; (map
;;  displayln
;;  (Loop-instr-list (list-ref repaired-instr-list 11)))

;; (map
;;  displayln
;;  repaired-instr-list)



 
 

