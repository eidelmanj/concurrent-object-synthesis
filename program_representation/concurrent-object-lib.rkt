#lang rosette/safe
(provide list-add
         list-remove
         add-entry
         copy-map
         add-entry
         remove-mapped
         get-mapped
         update-mapped
         amap-get
         amap-get-arg
         amap-remove
         amap-contains-arg
         amap-remove-arg
         AMap-entry
         amap-put
         amap-putIfAbsent
         amap-contains
         )


;; This is a list based implementation of a map 
;; This map is used to represent the program environment when simulating a C program
(define (copy-map m)
  (map (lambda (x) x) m))

;; Map put function
(define (add-entry m key val)
  (if (empty? m)
      (list (cons key val))
      (append (list (first m)) (add-entry (rest m) key val))))

;; Map remove function
(define (remove-mapped m key)
  (if (empty? m)
      `()
      (if (equal? (car (first m)) key)
          (remove-mapped (rest m) key)
          (append (list (first m)) (remove-mapped (rest m) key)))))

;; Map get function
(define (get-mapped m key)

  (if (empty? m)
      `()
      
      (if (equal? (car (first m)) key)
          (cdr (first m))
          (get-mapped (rest m) key))))
                      
;; Map update function
(define (update-mapped m key val)
  (let ([new-m (remove-mapped m key)])
    (add-entry new-m key val)))




;; The following is a representation of the Java ConcurrentHashmap library object
;; Each function takes as input an environment (a list of pairs) and returns an environment
;; with the necessary changes to the requested data structure
(define-struct AMap-entry (key val) #:transparent)
;; (define test-amap (list (AMap-entry "thing" 12) (AMap-entry "other" "thing") (AMap-entry "thing" 25)))


(define (amap-get e m-name key target-var)
  (let ([amap (get-mapped e m-name)])
    (let ([all-matches (filter (lambda (entry) (equal? (AMap-entry-key entry) key)) amap)])
      (cond
        [(empty? all-matches) (update-mapped e target-var "Null")]
        [else (update-mapped e target-var (AMap-entry-val (first all-matches)))]))))

(define (amap-get-arg e m-name key-store target-var)
  (let ([key (get-mapped e key-store)])
    (amap-get e m-name key target-var)))
 
;; (define amap-test-env (list (cons "ref" "other") (cons "m1" test-amap)))
;; (amap-get amap-test-env "m1" "th" "z0")

(define (amap-remove e m-name key target-var)
  (let ([old-amap (get-mapped e m-name)])
    (let ([new-amap (filter (lambda (entry) (not (equal? (AMap-entry-key entry) key))) old-amap)]
          [ret-val
           (let ([all-matches (filter (lambda (entry) (equal? (AMap-entry-key entry) key)) old-amap)])
             (cond
               [(empty? all-matches) "Null"]
               [else (AMap-entry-val (first all-matches))]))])
      (update-mapped (update-mapped e target-var ret-val) m-name new-amap))))

(define (amap-remove-arg e m-name key-store target-var)
  (let ([key (get-mapped e key-store)])
    (amap-remove e m-name key target-var)))

;; (amap-remove amap-test-env "m1" "thing" "z0")

(define (amap-put e m-name key val)
  (let ([old-amap (get-mapped e m-name)])
    (let ([new-amap
           (append
            (filter (lambda (entry) (not (equal? (AMap-entry-key entry) key))) old-amap)
            (list (AMap-entry key val)))])
      (update-mapped e m-name new-amap))))

(define (amap-put-arg e m-name key-store val-store)
  (let ([key (get-mapped e key-store)] [val (get-mapped e val-store)])
    (amap-put e m-name key val)))


(define (amap-putIfAbsent e m-name key val target-var)
  (let ([old-amap (get-mapped e m-name)])
    (let ([all-matches (filter (lambda (entry) (equal? (AMap-entry-key entry) key)) old-amap)])
      (cond
        [(empty? all-matches)
         (let ([new-amap
                (append
                 (filter (lambda (entry) (not (equal? (AMap-entry-key entry) key))) old-amap)
                 (list (AMap-entry key val)))])
           (update-mapped (update-mapped e target-var val) m-name new-amap))]
        [else (update-mapped e target-var "Null")]))))

(define (amap-putIfAbsent-arg e m-name key-store val-store target-var)
  (let ([key (get-mapped e key-store)] [val (get-mapped e val-store)])
    (amap-putIfAbsent e m-name key val target-var)))


(define (amap-contains e m-name key target-var)
  (let ([old-amap (get-mapped e m-name)])
    (update-mapped e target-var (not (empty? (filter (lambda (entry) (equal? (AMap-entry-key entry) key)) old-amap))))))


(define (amap-contains-arg e m-name key-store target-var)
  (let ([key (get-mapped e key-store)])
    (amap-contains e m-name key target-var)))





;; Similar to the hashmap implementation above, except with behaviour of linked list
(define (list-get env l-name idx ret-var)
  ;; (display "LIST_GET\n")
  (letrec ([list-get-helper
         (lambda (l idx)
           (if (= 0 idx)
               (cond 
               [(empty? l) "Null"]
               [else (first l)])
               (list-get-helper (rest l) (- idx 1))))])
    (add-entry env ret-var (list-get-helper (get-mapped env l-name) idx))))


(define (list-remove env l-name idx ret-var)
  ;; (display "LIST_REMOVE\n")

  
  (letrec ([list-remove-helper
            (lambda (l idx)
              (if (= 0 idx)
                  (if (not (empty? l))
                      (append `() (rest l))
                      `())
                  (append (list (first l)) (list-remove-helper (rest l) (- idx 1)))))])
    
    (let ([ret-val (get-mapped (list-get env l-name idx "tmp") "tmp")]
          [updated-env (update-mapped env l-name
                                      (list-remove-helper (get-mapped env l-name) idx))])
      
      ;; (display (add-entry updated-env ret-var ret-val)) (display "-DONE\n")
      (add-entry updated-env ret-var ret-val))))
          
        
(define (list-add env l-name val)
  ;; (display "LIST_ADD\n")
  (update-mapped env l-name (append (get-mapped env l-name) (list (get-mapped env val)))))


