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


(define (complete-racket-translation instr-list arg-list)
  ;; (displayln "translating line...")
  (cond
    [(empty? instr-list) `()]

    [(and
      (Run-method? (first instr-list))
      (equal?
       (substring (Run-method-method (first instr-list)) 0 3)
       "new")
      )






     


     (append
      (list
       (Create-var (get-return-var (Run-method-args (first instr-list)))
                   (substring (Run-method-method (first instr-list)) 3 (string-length
                                                                        (Run-method-method (first instr-list)))))
       (Set-var (remove-uneccessary-get-var (Run-method-ret (first instr-list)))
                (New-struct (substring (Run-method-method (first instr-list))
                                       3
                                       (string-length
                                        (Run-method-method (first instr-list))))
                            (Run-method-args (first instr-list)))))
      (complete-racket-translation (rest instr-list) arg-list))]

    
    [(Set-var? (first instr-list))
     (append
      (list
       (Set-var (Set-var-id (first instr-list)) (transform-arguments (Set-var-assignment (first instr-list)) arg-list)))
      (complete-racket-translation (rest instr-list) arg-list))]
    [(and
      (Create-var? (first instr-list))
      (Set-var? (Create-var-id (first instr-list))))
     (append
      (list
       (Create-var (Set-var-id (Create-var-id (first instr-list))) (Create-var-type (first instr-list)))
       (Create-var-id (first instr-list)))
      (complete-racket-translation (rest instr-list) arg-list))]

    [(Run-method? (first instr-list))
     (append
      (list
       (Run-method
        (Run-method-method (first instr-list))
        (translate-arg-list (Run-method-args (first instr-list)) arg-list)
        (get-return-var (Run-method-args (first instr-list)))))
      (complete-racket-translation (rest instr-list) arg-list))]

    [(Return? (first instr-list))
     (append
      (list
       (Return (Get-var "_out")))
      (complete-racket-translation (rest instr-list) arg-list))]

    [(Set-pointer? (first instr-list))
     (append
      (list
       (Set-pointer
        (remove-uneccessary-get-var (Set-pointer-id (first instr-list)))
        (remove-uneccessary-get-var (Set-pointer-type (first instr-list)))
        (remove-uneccessary-get-var (Set-pointer-offset (first instr-list)))
        (remove-uneccessary-get-var (Set-pointer-val (first instr-list)))))
      (complete-racket-translation (rest instr-list) arg-list))]

    [else
     (append
      (list (first instr-list))
      (complete-racket-translation (rest instr-list) arg-list))]
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
  (complete-racket-translation (Method-instr-list (first lst)) (Method-args (first lst)))
  (Argument-type (last (Method-args (first lst))))))






;;;; Now lets try with the library itself




(define lib-str   "

void add(List* l, int newVal) {

  newNode(newN_s186);
  newN_s186->val = newVal;
  if ((l->first) == (NULL)) {
    l->first = newN_s186;
    l->last = newN_s186;
  } else {
    l->last->next = newN_s186;
    l->last = newN_s186;
  }
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
  while ((cur) != (NULL)) {
    if ((0) == ((idx - 1))) {
      toRemove = cur->next;
      cur->next = cur->next->next;
      if ((l->last) == (toRemove)) {
        l->last = cur->next;
      }
      _out = toRemove->val;
      return;
    }
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


(define lst2 (let*
                ((test-program lib-str)
                 (input (open-input-string test-program)))
              (translate (simple-math-parser (lex-this simple-math-lexer input)))))













(displayln (Method-id (first lst2)))



(argument-definitions (Method-args (first lst2)))


(define (combine-all-repairs instr-list arg-list)
  (append
   (argument-definitions arg-list)
   (complete-racket-translation instr-list arg-list)))


(map
 (lambda (l)
   (displayln l))
 (combine-all-repairs (Method-instr-list (first lst2)) (Method-args (first lst2))))

