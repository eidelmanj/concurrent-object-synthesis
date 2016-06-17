#lang racket

(require (only-in "../cex_generalization/c-program-representation.rkt"
                  create-announcement-version)
         "../program_representation/simulator-structures.rkt"
         "error-trace.rkt")

(define test-library
  (list
   (Method
    "get"
    (list "Node" "int")
    "int"
    (create-announcement-version 
     `(,(Lock 1 (None))
       ,(Create-var "cur" "Node" (None))
       ,(Set-var "cur" (Get-argument 0) (None))
       ,(Loop (And (Not (Is-none? (Get-var "cur")))
                   (Not (Equal (Dereference "cur" "Node" "key") (Get-argument 1))))
              `(,(Set-var "cur" (Dereference "cur" "Node" "next") (None))))
       ,(Single-branch (Is-none? (Get-var "cur"))
                       `(,(Unlock 1 (None)) ,(Return 0 (None))))
       ,(Unlock 1 (None))
       ,(Return (Dereference "cur" "Node" "val") (None)))))     
   
   (Method
    "contains"
    (list "Node" "int")
    "int"
    `(,(Create-var "valu" "int" (None))
      ,(Run-method "get" `(,(Get-argument 0) ,(Get-argument 1)) "valu")
      ,(Return (Equal (Get-var "valu") 0) (None))))
   
   #;
   (Method
    "remove"
    (list "Node" "int")
    "int"
    `(,(Lock 1 (None))
      ,(Create-var "cur" "Node" (None))
      ,(Create-var "prevNode" "Node" (None))
      ,(Create-var "oldVal" "Node" (None))
      
      ,(Set-var "cur" (Get-argument 0) (None))
      ,(Single-branch (Equal (Get-var "cur") 0)
                      `(,(Unlock 1 (None)) ,(Return 0 (None))))
      
      ,(Set-var "oldVal" (Dereference "cur" "Node" "val") (None))
      ,(Set-var "prevNode" (Get-argument 0) (None))
      
      ,(Loop (And (Not (Equal (Dereference (Dereference "cur" "Node" "next") "Node" "key")
                              (Get-argument 1)))
                  (Not (Equal (Get-var "cur") 0)))
             `(,(Set-var "oldVal" (Dereference "cur" "Node" "val") (None))
               ,(Set-var "prevNode" (Get-var "cur") (None))           
               ,(Set-var "cur" (Dereference "cur" "Node" "next") (None))))
      ,(Single-branch (Equal (Get-var "cur") 0)
                      `(,(Unlock 1 (None)) ,(Return 0 (None))))
      
      ,(Set-var "oldVal" (Dereference "cur" "Node" "val") (None))
      ,(Set-pointer "prevNode" "Node" "next" (Dereference "cur" "Node" "next") (None))
      ,(Unlock 1 (None))
      ,(Return (Get-var "oldVal") (None))))
   
   (Method
    "extension"
    (list "Node" "int")
    "int"
    `(,(Create-var "val" "int" (None))
      ,(Create-var "found" "int" (None))
      ,(Set-var "val" 0 (None))
      ,(Run-method "contains" `(,(Get-argument 0) ,(Get-argument 1)) "found")
      ,(Single-branch (Equal (Get-var "found") 0)
                      `(,(Run-method "get" `(,(Get-argument 0) ,(Get-argument 1)) "val")
                        ,(Run-method "get" `(,(Get-argument 0) ,(Get-argument 1)) "")))
      ,(Return (Get-var "val") (None))))))

(error-traces test-library "extension" (make-hash `(("Node" . (,(Get-var "shared"))))))
