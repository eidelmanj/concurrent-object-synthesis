#lang racket

(require readline/readline)
(require "parser.rkt")
(require "invariants.rkt")
(require racket/string)


;; (define (synthesize prgrm verifyer c-transformers s-transformers)
;;   (let ([c-idx (+ (random (length c-transformers)) 1)] [s-idx (+ (random (length s-transformers)) 1)])
;;        5))

;; (synthesize 5 5 `("a" "b") `("d"))

;; (define-syntax get
;;   (syntax-rules (from in)
;;     [(get <expr> from <obj> in <prgrm>)
;;      (match (<obj>)
;;        [(<obj> <expr>) <prgrm>])]))
  

(define (all-false l)
  (if (empty? l)
      #t
      (and (not (first l)) (all-false (rest l)))))



(define (full-search prgrm s-verifier c-verifier c-transformers s-transformers all-shared getters setters)
  (let ([recurse-with-transformed
         (lambda (t)
           (let ([transformed-prgrm (t prgrm all-shared getters setters)])
             (if (string=? (pp transformed-prgrm) (pp prgrm))
                 #f
                 (full-search transformed-prgrm s-verifier c-verifier c-transformers s-transformers all-shared getters setters))))])

    (if (s-verifier prgrm) 
        (let ([c-search-results (c-transform-search prgrm c-verifier c-transformers all-shared getters setters)])
          (let ([sanitized-c-search-results (filter (lambda (i) (not (boolean? i)))
                                                    (flatten c-search-results))])
            
            (if (empty? sanitized-c-search-results)
                (map recurse-with-transformed s-transformers)
                sanitized-c-search-results)))
        (map recurse-with-transformed s-transformers)
        )))
                ;; (for ([s s-transformers])
                ;;   (recurse-with-transformed s))
                ;; sanitized-c-search-results))))))
         
                 

(define (c-transform-search prgrm verifier c-transformers all-shared getters setters [already-checked #f])

  (let ([recurse-with-transformed
         (lambda (t)

           (let ([transformed-prgrm (t prgrm all-shared getters setters)])
             (if (string=? (pp transformed-prgrm) (pp prgrm))
                 #f
                 (c-transform-search transformed-prgrm verifier c-transformers all-shared getters setters))))])
    (if already-checked (map recurse-with-transformed c-transformers)
        
        (if (verifier prgrm) (begin (display (string-append "FOUND SOLUTION: " (format "~a" (verifier prgrm)))) prgrm)
            (map recurse-with-transformed c-transformers)))))


(define (verify? prgrm)
  (display (pp prgrm))
  (let ([resp (readline ">>>")])
    (if (string=? resp "y") #t #f)))


(define (s-transformer-1 prgrm all-shared getters setters)

  (let
      ([all-gets (all-shared-accesses all-shared getters prgrm)]
       [all-sets (all-shared-accesses all-shared setters prgrm)]

      ;; Gets the first get-set pair where the set is after the get
      [find-match
       (lambda (gets sets)
         (if (empty? gets)
             `()
             (match (first gets)
               [(access-record v1 v2 o args cnt type)
                
                (cons (first gets) (filter (lambda (s) (match s [(access-record _ _ _ _ cnt2 _) (> cnt2 cnt)]))
                                           sets))])))])
    
    
    (let ([matching-records (find-match all-gets all-sets)])
      
      (if (> (length matching-records) 1)
          (let ([new-prgrm (remove-access prgrm (first (rest matching-records)))])
            (replace-node new-prgrm (first matching-records)
                          (match (first matching-records)
                            [(access-record v1 v2 _ args cnt type)
                             (match (first (rest matching-records))
                               [(access-record _ _ o _ _ _) (access-record v1 v2 o args cnt type)])])))
          
          prgrm))))
    


(define (c-transformer-1 prgrm all-shared getters setters)

  (let ([untried
         (lambda (assignment)
           (match assignment
             [(access-record v1 v2 o args cnt type) (if (= type 0) #t #f)]))])

                   
  (let ([all-gets (all-shared-accesses all-shared getters  prgrm)])
    (let ([all-sets (all-shared-accesses all-shared setters prgrm)])
    

    
      (let ([untried-gets (filter untried all-gets)] [untried-sets (filter untried all-sets)])
        (if (or (empty? untried-gets) (empty? untried-sets)) prgrm 
            (let ([first-assign (first untried-gets)] [first-set (first untried-sets)])
              (let ([new-prgrm (replace-node prgrm first-assign (increment-type first-assign))])
                (replace-node prgrm first-set (increment-type first-set))))))))))
      
      
(define (increment-type node)
  (match node
    [(access-record v1 v2 o args cnt type) (access-record v1 v2 o args cnt (+ type 1))]))

(define (access-record-to-node record)
  (match record
    [(access-record v1 v2 o args cnt type) (access-record v1 v2 o args cnt type)]))

(define (replace-node prgrm original replacement)
  (let ([orig-id (match original [(access-record v1 v2 o args cnt type) cnt])]
        [replacement-node (access-record-to-node replacement)])
    (match prgrm
      [(start-node u p) (start-node u (replace-node p original replacement))]
      [(program-node stmt next) (program-node (replace-node stmt original replacement) (replace-node next original replacement))]
      [(if-stmt c) (if-stmt (replace-node c original replacement))]
      [(if-node e p1 p2) (if-node e (replace-node p1 original replacement) (replace-node p2 original replacement))]
      [(if-root c) (if-root (replace-node c original replacement))]
      [(assign-obj v1 v2 o cnt type) (if (= orig-id cnt) replacement-node (assign-obj v1 v2 o cnt type))]
      [(access-record v1 v2 o args cnt type) (if (= orig-id cnt) replacement-node (access-record v1 v2 o args cnt type))]



      [(single-var v) (single-var v)]
      [(return-node v) (return-node v)]
      [(empty-node) (empty-node)]
      [_ prgrm])))


(define (remove-access prgrm access)
  (let ([check-identity
         (lambda (s id)
           (match s
             [(assign-obj v1 v2 o cnt type) (= cnt id)]
             [_ #f]))])
    (let ([access-id (match access [(access-record v1 v2 o args cnt type) cnt])])
      (match prgrm
        [(start-node u p) (start-node u (remove-access p access))]
        [(program-node stmt next) (if (check-identity stmt access-id)
                                      (remove-access next access)
                                      (program-node stmt (remove-access next access)))]
        [_ prgrm]))))
      ;; [(start-node u p) (start-node u (replace-node p original replacement))]
      ;; [(program-node stmt next) (program-node (replace-node stmt original replacement) (replace-node next original replacement))]
      ;; [(if-stmt c) (if-stmt (replace-node c original replacement))]
      ;; [(if-node e p1 p2) (if-node e (replace-node p1 original replacement) (replace-node p2 original replacement))]
      ;; [(if-root c) (if-root (replace-node c original replacement))]
      ;; [(assign-obj v1 v2 o cnt type) (if (= orig-id cnt) replacement-node (assign-obj v1 v2 o cnt type))]
      ;; [(access-record v1 v2 o args cnt type) (if (= orig-id cnt) replacement-node (access-record v1 v2 o args cnt type))]

      ;; [(single-var v) (single-var v)]
      ;; [(return-node v) (return-node v)]
      ;; [(empty-node) (empty-node)])))
                                


(define (synthesize-methods prgrm seq-verifier conc-verifier c-transformers s-transformers all-shared getters setters)
  (match prgrm
    [(start-node u p) (synthesize-methods p seq-verifier conc-verifier c-transformers s-transformers all-shared getters setters)]
    [(program-node stmt next) (list (synthesize-methods stmt seq-verifier conc-verifier c-transformers s-transformers all-shared getters setters)
     (synthesize-methods next seq-verifier conc-verifier c-transformers s-transformers all-shared getters setters))]
    [(method-root (method-node tp nm vlist p)) (full-search p seq-verifier conc-verifier c-transformers s-transformers all-shared getters setters)]
    [_ #f]))




(define test-str "
Node n;
int key, val;
Integer q;
Node w;
n = l.get(idx);
key = n.key;
val = n.val;
q=m.putIfAbsent(key, val);
w=l.remove(idx);
return n;")

(define test-ctx "
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;


class Node {
    int val;
    int key;
    public Node(int key, int val) {
        this.val = val;
        this.key = key;
    }
}

public class Test {
    public CopyOnWriteArrayList<Node> l = new CopyOnWriteArrayList<Node>();
    public ConcurrentHashMap<Integer, Integer> m = new ConcurrentHashMap<Integer, Integer>();
    
    public Node something(int idx) {
MARK
    }



    public static void main(String args[]) {
TESTS
    }
}


")

(define (test-sequential prgrm-str context test-set)
  (let ([in-ctx (string-replace context "MARK" prgrm-str)])
    (let ([with-tests (string-replace in-ctx "TESTS" test-set)])
      (with-output-to-file "Test.java" (lambda () (printf with-tests)) #:exists 'replace)
      (define p (open-output-string))
      ;; (current-output-port p)
      (let ([a (process/ports (current-output-port) (current-input-port) (current-output-port) "javac Test.java")])
        ((list-ref a 4) `wait)
        (let
            ([b (process/ports (current-output-port) (current-input-port) (current-output-port) "java Test 1> output.tmp")])
          ((list-ref b 4) 'wait)
          (let ([output-str (file->string "output.tmp")])
            (not (list? (regexp-match "ERROR" output-str)))))))))

      
(define (insert-at-pos l a)
  (lambda (n)
    (if (= n 0)
        (append (list a) l)
        (append (list (first l)) ((insert-at-pos (rest l) a) (- n 1))))))

(define (shuffle-in l a)
  (map (insert-at-pos l a)
       (range (- (length l) 1))))
       
        




(define (test-conc prgrm-str context test-set)
  (define track (void))
  ;; All possible variations of the program with a single yield
  (let ([insert-yields
         (lambda (str)
           
           (shuffle-in (string-split str "\n") "Thread.yield();"))])
    (let ([all-programs
           (map combine-string (insert-yields prgrm-str))])
      (let ([conc-programs (map
       (lambda (new-prgrm)
         (string-append
          (string-replace
           (string-replace context "MARK" new-prgrm)
           "TESTS" test-set)))
       all-programs)])


        (set! track 0)
        (or-list (map
         (lambda (s)

           (if (directory-exists? (string-append "tmp" (format "~a" track)))
               (void)
               (make-directory (string-append "tmp" (format "~a" track))))

           (current-directory (string-append "tmp" (format "~a" track)))
           (with-output-to-file "Test.java"
                                               (lambda () (printf s)) #:exists 'replace)
           (set! track (+ track 1))

           ;; (current-output-port p)
           (let ([a (process/ports (current-output-port) (current-input-port) (current-output-port) "javac Test.java")])
             ((list-ref a 4) `wait)
             (let
                 ([b (process/ports (current-output-port) (current-input-port) (current-output-port) "java Test 1> output.tmp")])
               ((list-ref b 4) 'wait)
               (let ([output-str (file->string "output.tmp")])
                 (list? (regexp-match "ERROR" output-str))))))
         conc-programs))))))

           
(define (or-list l)
  (if (empty? l)
      #f
      (or (first l) (or-list (rest l)))))


(define (combine-string l)
  (if (empty? l)
      ""
      (string-append (first l) (combine-string (rest l)))))

(define str "{shared {m,l} getters {get} setters {remove}}
Node something(int idx) {
Node n;
Node w;
Integer q;
n = l.get(idx);
int key;
int val;
key = n.key;
val = n.val;
q=m.putIfAbsent(key, val);
w=l.remove(idx);
return n;
}
")

(define all-tests "

	Test t = new Test();
	t.l.add(new Node(1, 2));
	t.something(0);
	if (!t.l.isEmpty()) System.out.println(\"ERROR\");
	if (!t.m.containsKey(1)) System.out.println(\"ERROR\");
	else {
	    if (t.m.get(1)!=2) System.out.println(\"ERROR\");
	} 
")


(define all-conc-tests "


	l.add(new Node(1, 2));
	something(0);
	if (!l.isEmpty()) System.out.println(\"ERROR\");
	if (!m.containsKey(1)) System.out.println(\"ERROR\");
	else {
	    if (m.get(1)!=2) System.out.println(\"ERROR\");
	} 
")










(define conc-ctx
"
import java.io.PrintStream;
import java.io.FileOutputStream;
import java.io.File;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;


class Node {
    int val;
    int key;
    public Node(int key, int val) {
        this.val = val;
        this.key = key;
    }
}

public class Test {
    public LinkedList<Node> l = new LinkedList<Node>();
    public ConcurrentHashMap<Integer, Integer> m = new ConcurrentHashMap<Integer, Integer>();
    
    public Node something(int idx) {

MARK	

    }


    class T1 implements Runnable{

	@Override
	public void run() {


	TESTS

	}
    }

    private void test() {
	T1 main = new T1();


	new Thread(main).start();

    }
    
    public static void main(String args[]){
        try {
        PrintStream out = new PrintStream(new FileOutputStream(\"output.tmp\"));
        System.setErr(out);
        System.setOut(out);

        } catch(Exception e) {System.out.println(\"Nooooo\");}
	Test test = new Test();
	for (int i=0; i< 1; i++)
	    test.test();


    }

    
}


")

;; (test-conc test-str conc-ctx all-conc-tests)


(define (seq-test-1 prgrm) (test-sequential (pp prgrm) test-ctx all-tests))
(define (conc-test-1 prgrm) (test-conc (pp prgrm) conc-ctx all-conc-tests))
;; (display (test-sequential test-str test-ctx all-tests))

(let ([input (open-input-string str)])
  (let ([strt (begin-parse (lex-this simple-math-lexer input))])
    ;; (display (pp (remove-access strt (access-record "w" "l" "remove" (list "idx") 4 0))))))
    ;; (display (pp (s-transformer-1 strt)))))
    ;;(full-search strt verify? (list c-transformer-1) (list s-transformer-1))))

    (display (pp (first (flatten(synthesize-methods strt seq-test-1 conc-test-1
                        (list c-transformer-1)
                        (list s-transformer-1)
                        (shared-vars strt)
                        (all-getters strt)
                        (all-setters strt))))))))
