#lang racket

(require "../program_representation/simulator-structures.rkt")
(provide
 thread3
 test
 mooly-test
 mooly-sketch-test)



(define thread3
  (Thread-list
   (list
    (Run-method "get" (list 1 "test") "ret" ))))



(define test
  (Thread-list
   (list
    ;; (Create-var "ret1" "int" (None))
    (Create-var "ret2" "int" )
    (Create-var "ret3" "int" )
    (Run-Method "push" (list (Get-var "shared") 1 5) "ret1" 0 )
    (Run-Method "push" (list (Get-var "shared") 2 5) "ret1" 0 )
    (Run-Method "push" (list (Get-var "shared") 3 7) "ret1" 0 )
    (Run-Method "get" (list (Get-var "shared") 2 ) "ret3" 0)
    ;; (Run-Method "contains" (list (Get-var "shared") 1) "ret2" 1)
    (Run-Method "remove" (list (Get-var "shared") 1) "ret1" 0))))
  






;;; Mooly's error
(define mooly-test
  (Thread-list
   (list
    (Run-Method "push" (list (Get-argument 0) 1 5) "throwaway" 0)
    (Create-var "val" "int" )
    (Create-var "found" "int" )
    (Set-var "val" 0 )
    (Run-Method "contains" (list (Get-argument 0) (Get-argument 1)) "found" 0)
    (Assume-simulation (Not (Equal (Get-var "found") 0)))

    
    (Run-Method "get" (list (Get-argument 0) (Get-argument 1)) "val" 0)
    (Run-Method "remove" (list (Get-argument 0) (Get-argument 1)) "ret2" 1)
    (Run-Method "remove" (list (Get-argument 0) (Get-argument 1)) "throwaway" 0)
    (Set-var "ret1" (Get-var "val") ))))



(define mooly-sketch-test
  (Thread-list
   (list
    (Run-Method "push" (list (Get-argument 0) 1 5) "throwaway" 0)
    (Create-var "val" "int" )
    (Create-var "found" "int" )
    (Set-var "val" 0 )
    (Run-Method "contains" (list (Get-argument 0) (Get-argument 1)) "found" 0)
    (Assume-simulation (Not (Equal (Get-var "found") 0)))

    (Meta-branch 0
                 (list
                  (Run-Method "get" (list (Get-argument 0) (Get-argument 1)) "val" 0)
                  (Run-Method "remove" (list (Get-argument 0) (Get-argument 1)) "ret2" 1)
                  (Run-Method "remove" (list (Get-argument 0) (Get-argument 1)) "throwaway" 0))
                 (list
                  (Create-var "loop-break" "int" )
                  (Set-var "loop-break" #f )
                  (Maybe-loop 1 (Not (Get-var "loop-break"))
                              (list
                               (Run-Method-instr-id "get" (list (Get-argument 0) (Get-argument 1)) "val" 0 0)
                               (Run-Method-instr-id "remove" (list (Get-argument 0) (Get-argument 1)) "throwaway" 0 1))
                              (list
                               (Run-Method "remove" (list (Get-argument 0) (Get-argument 1)) "ret2" 1))

                              (list
                               (Run-Method "get" (list (Get-argument 0) (Get-argument 1)) "val" 0)
                               (Run-Method "remove" (list (Get-argument 0) (Get-argument 1)) "ret2" 1)
                               (Run-Method "remove" (list (Get-argument 0) (Get-argument 1)) "throwaway" 0))



                              (Hole 0 (list) 1))))

                              

                              
                        

                 
    (Set-var "ret1" (Get-var "val")))))













;; Example threads 
(define thread1
  (Thread-list
   (list
    (Run-method "remove" (list 1 "test") "ret" ))))


(define thread2
  (Thread-list
   (list
    (Run-method "extension" (list 1 "test") "ret2" ))))
