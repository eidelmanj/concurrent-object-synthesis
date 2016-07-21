;; COMES FROM c-program-representation.rkt
;; (define (all-equivalent-interleavings interleaving-full thread1 thread2 library)

;;   (define (all-equivalent-interleavings-helper interleavings thread1-runs thread2-runs)
;;     (display "all-equivalent-interleavings-helper\n")
;;     (cond
;;       [(empty? interleavings)
;;        (list (list))]
;;       [else
;;        (let ([t-id (Get-thread-id (first interleavings))]
;;              [same-id-thread1 (filter (lambda (l) (and (not (empty? l)) (equal? (Get-instr-id (first l))
;;                                                                                 (Get-instr-id (first interleavings)))))
;;                                       thread1-runs)]
;;              [same-id-thread2 (filter (lambda (l) (and (not (empty? l)) (equal? (Get-instr-id (first l))
;;                                                                                 (Get-instr-id (first interleavings)))))
;;                                       thread2-runs)]

;;              [maybe-loop-thread1 (filter (lambda (l) (and (not (empty? l)) (None? (Get-instr-id (first l)))))
;;                                          thread1-runs)]
;;              [maybe-loop-thread2 (filter (lambda (l) (and (not (empty? l)) (None? (Get-instr-id (first l)))))
;;                                          thread2-runs)])
         


;;          ;; (display "same-id-thread1: ") (display  (map (lambda (l) (rest l)) same-id-thread1)) (display "\n")
;;          ;; (display "maybe-loop-thread1: ") (display maybe-loop-thread1) (display "\n")




;;          (cond
;;            [ (equal? t-id 0) 
;;             (append
;;              (map (lambda (l) (if (empty? l) `()
;;                                   (append (list (first l))

;;                                           (first (all-equivalent-interleavings-helper
;;                                                             (rest interleavings)
;;                                                             (list (rest l))
;;                                                             thread2-runs)))))
;;                   same-id-thread1)




;;              (map (lambda (l)
;;                     (display l) (display "\n")
;;                     (if (> 2 (length l)) `()
;;                         (all-equivalent-interleavings-helper (find-appropriate-subseq interleaving-full (first (rest l)))
;;                                                   (rest (list l))
;;                                                   thread2-runs)))
;;                   maybe-loop-thread1))]
           
;;            [else
;;             (list (list))]))]))

  
            



  
;;   (let ([thread1-runs (thread-runs thread1 library 0 "")]
;;         [thread2-runs (thread-runs thread2 library 1 "")])

;;     (all-equivalent-interleavings-helper interleaving-full thread1-runs thread2-runs)))
