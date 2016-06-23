#lang racket

(require "../program_representation/simulator-structures.rkt"
         "error-trace.rkt"
         "backtracking.rkt")

(define test-library
  (list
   (Method
    "get"
    (list "Node" "int")
    "int"
    `(,(Lock 1)
      ,(Create-var "cur" "Node")
      ,(Set-var "cur" (Get-argument 0))
      ,(Loop (And (Not (Is-none? (Get-var "cur")))
                  (Not (Equal (Dereference "cur" "Node" "key") (Get-argument 1))))
             `(,(Set-var "cur" (Dereference "cur" "Node" "next"))))
      ,(Single-branch (Is-none? (Get-var "cur"))
                      `(,(Unlock 1) ,(Return 0)))
      ,(Unlock 1)
      ,(Return (Dereference "cur" "Node" "val"))))

   (Method
    "contains"
    (list "Node" "int")
    "int"
    `(,(Create-var "valu" "int")
      ,(Run-method "get" `(,(Get-argument 0) ,(Get-argument 1)) "valu")
      ,(Return (Equal (Get-var "valu") 0))))

   (Method
    "extension"
    (list "Node" "int")
    "int"
    `(,(Create-var "val" "int")
      ,(Create-var "found" "int")
      ,(Set-var "val" 0)
      ,(Run-method "contains" `(,(Get-argument 0) ,(Get-argument 1)) "found")
      ,(Single-branch (Equal (Get-var "found") 0)
                      `(,(Run-method "get" `(,(Get-argument 0) ,(Get-argument 1)) "val")
                        ,(Run-method "get" `(,(Get-argument 0) 25) "test")))
      ,(Return (Get-var "val"))))))

(bound 1)
(error-traces test-library "extension" (make-hash `(("Node" . (,(Get-var "shared"))))))
