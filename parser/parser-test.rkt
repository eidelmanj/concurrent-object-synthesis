#lang racket

(require rackunit
         "parser.rkt")


(let ((input (open-input-string "{shared {} getters {} setters {}} if (1) { x=5; } else {y=7; } ")))
  (check-equal? (pp (begin-parse (lex-this simple-math-lexer input)))
                "if (1) {x=5;\n} else{y=7;\n}\n"
                "if-else clause"))


(let ((input (open-input-string "{shared {} getters {} setters {}} x=5; x=6;")))
  (check-equal? (pp (begin-parse (lex-this simple-math-lexer input)))
                "x=5;\nx=6;\n"
                "two assignment statements"))


(let ((input (open-input-string "{shared {} getters {} setters {}} int test (int x, bool y ) { x=5; }")))
  (check-equal? (pp (begin-parse (lex-this simple-math-lexer input)))
                "int test(int x, bool y){\nx=5;\n}\n\n"
                "method declaration"))


(let ((input (open-input-string "{shared {} getters {} setters {}} test(x, y);")))
  (check-equal? (pp (begin-parse (lex-this simple-math-lexer input)))
                "test(x, y);\n"
                "Function call"))



;; (let ((input (open-input-string "{shared {} getters {} setters {}} x.test(x, y);")))
;;   (check-equal? (pp (begin-parse (lex-this simple-math-lexer input)))
;;                 "x.test(x, y);\n"
;;                 "Object function call"))


(let ((input (open-input-string "{shared {} getters {} setters {}} x=y.test(x, y);")))
  (check-equal? (pp (begin-parse (lex-this simple-math-lexer input)))
                "x=y.test(x, y);\n"
                "Object access assignment"))


(let ((input (open-input-string "{shared {} getters {} setters {}} x=y.test(x, y);")))
  (check-equal? (pp (begin-parse (lex-this simple-math-lexer input)))
                "x=y.test(x, y);\n"
                "Object access assignment"))

;; (define str (open-input-string "{shared {m,l} getters {get} setters {}}
;; if (n==null) {return null;}
;; key = n.key;
;; val = n.val;
;; m.putIfAbsent(key, val);
;; l.remove(idx);
;; return n;
;; "))

;; (pp (begin-parse (lex-this simple-math-lexer str)))
;; (test-parse str)



(let ((input (open-input-string "{shared {} getters {} setters {}}
int test (int x, bool y ) { x=5; foo(m, a, b); putIfAbsent(b, c); remove(a); return x; }")))
  (display (pp (begin-parse (lex-this simple-math-lexer input)))))
