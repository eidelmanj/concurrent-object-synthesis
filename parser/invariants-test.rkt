#lang racket

(require rackunit
         "parser.rkt"
         "invariants.rkt")

(let ([arg-list (arg-node (arg-decl "x") (arg-add-node (arg-decl "y") (empty-node))) ])
  (check-equal? (all-args arg-list) `("x" "y")))


(let ([arg-list (arg-node (arg-decl "x") (arg-add-node (arg-decl "y") (empty-node))) ])
  (let ([u-node (user-input-node arg-list (empty-node) (empty-node))])
    (let ([s-node (start-node u-node (empty-node))])
      (check-equal? (shared-vars s-node) `("x" "y")))))


(let ([arg-list (arg-node (arg-decl "x") (arg-add-node (arg-decl "y") (empty-node))) ])
  (let ([u-node (user-input-node (empty-node) arg-list (empty-node))])
    (let ([s-node (start-node u-node (empty-node))])
      (check-equal? (all-getters s-node) `("x" "y")))))

(let ([arg-list (arg-node (arg-decl "x") (arg-add-node (arg-decl "y") (empty-node))) ])
  (let ([u-node (user-input-node (empty-node) (empty-node) arg-list)])
    (let ([s-node (start-node u-node (empty-node))])
      (check-equal? (all-setters s-node) `("x" "y")))))

;; (let ((input (open-input-string "{shared {} getters {} setters {}} x=y.test; z=g.blah;")))
(let ((input (open-input-string "{shared {} getters {} setters {}} x=y.test(a, b); z=g.blah(c, d);")))
  (let ([get-program-node
         (lambda (start)
           (match start
             [(start-node u p) p]))])
    
    (let ([prgrm (get-program-node (simple-math-parser (lex-this simple-math-lexer input)))])
      ;; (all-get-assignments (list "x") (list "test") prgrm))))
      (check-equal? (all-get-assignments (list "y" "g") (list "test" "blah") prgrm) (list (access-record "x" "y" "test" (list "a" "b") 0) (access-record "z" "g" "blah" (list "c" "d") 0))))))



(let ([e (var-exp "x")])
  (check-equal? (includes-vars (list "x" "y") e) #t))


;; TODO: Unit test for find-equalities
(let ((input (open-input-string "{shared {} getters {} setters {}} x=y.test(a, b); z=x;")))
  (let ([get-program-node
         (lambda (start)
           (match start
             [(start-node u p) p]))])
    (let ([prgrm (get-program-node (simple-math-parser (lex-this simple-math-lexer input)))])
      (check-equal? (find-equalities (list "x") prgrm) (list (cons "z" (var-exp "x")))))))



(let ([correct-answer (list (cons (access-record "x" "y" "test" (list "a" "b") 0) (list (cons "z" (var-exp "x")))))])
  (let ((input (open-input-string "{shared {} getters {} setters {}} x=y.test(a, b); z=x;")))
    (let ([get-program-node
           (lambda (start)
             (match start
               [(start-node u p) p]))])
      (let ([prgrm (get-program-node (simple-math-parser (lex-this simple-math-lexer input)))])
        (check-equal? (equivalent-vars (list (access-record "x" "y" "test" (list "a" "b") 0)) prgrm) correct-answer)))))




(let ([file-text (file->string "test_programs/if_test.java")])
  (let ([input (open-input-string file-text)])
    (let ([strt (simple-math-parser (lex-this simple-math-lexer input))])
      (let ([shared-list (shared-vars strt)] [getters (all-getters strt)] [setters (all-setters strt)])
        (match strt
          [(start-node u (program-node (if-stmt (if-root node)) next))
           (annotate-if node shared-list getters setters)]
          ;; TODO: Design a unit test for this....
          [_ "Test Failed!\n"])))))




