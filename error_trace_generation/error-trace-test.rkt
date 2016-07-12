#lang racket

(require "../program_representation/simulator-structures.rkt"
         "error-trace.rkt")

(define test-library
  (list
   (Method
    "push"
    '("Node" "int" "int")
    "int"
    `(,(Lock 1)
      ,(Create-var "cur" "Node")
      ,(Create-var "prev" "Node")
      ,(Create-var 'result "int")
      ,(Set-var "cur" (Get-argument 0))
      ,(Set-var "prev" (Get-argument 0))
      ,(Set-var 'result 0)
      ,(Loop (And (Not (Is-none? (Get-var "cur")))
                  (Not (Equal (Dereference "cur" "Node" "key") (Get-argument 1))))
             `(,(Set-var "prev" (Get-var "cur"))
               ,(Set-var "cur" (Dereference "cur" "Node" "next"))))
      ,(Single-branch
        (Is-none? (Get-var "cur"))
        `(,(Set-pointer "prev" "Node" "next"
                        (New-struct "Node" `(,(None)
                                             ,(Get-argument 1)
                                             ,(Get-argument 2)
                                             ,(None))))
          ,(Unlock 1)
          ,(Return 'result)))
      ,(Set-var 'result (Dereference 'cur 'Node 'val))
      ,(Set-pointer "cur" "Node" "val" (Get-argument 2))
      ,(Unlock 1)
      ,(Return 'result)))

   (Method
    "extension"
    '("Node" "int")
    "int"
    (list
     (Create-var 'val1 "int")
     (Create-var 'val2 "int")
     (Run-method "push" `(,(Get-argument 0) 1 10) 'val1)
     (Run-method "push" `(,(Get-argument 0) 1 20) 'val2)
     (Return (Add (Get-var 'val1) (Get-var 'val2)))))))

(bound 1)
(error-traces
 test-library
 "extension"
 (make-hash `(("Node" . (,(Get-var "shared"))))))
