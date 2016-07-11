#lang racket

(require "../program_representation/simulator-structures.rkt"
         "error-trace.rkt"
         "linearizable.rkt"
         "interpret.rkt")

(provide mooly-test-traces)

(define library
  (list
   (Method
    "push"
    '("Node" "int" "int")
    "int"
    `(,(Lock 1 )
      ,(Create-var "cur" "Node")
      ,(Create-var "prev" "Node")
      ,(Set-var "cur" (Get-argument 0))
      ,(Set-var "prev" (Get-argument 0))
      ,(Loop (And (Not (Is-none? (Get-var "cur")))
                  (Not (Equal (Dereference "cur" "Node" "key") (Get-argument 1))))
             `(,(Set-var "prev" (Get-var "cur"))
               ,(Set-var "cur" (Dereference "cur" "Node" "next"))))
      ,(Single-branch
        (Is-none? (Get-var "cur"))
        `(,(Set-pointer "prev" "Node" "next"
                        (New-struct "Node"
                                    `(,(None)
                                      ,(Get-argument 1)
                                      ,(Get-argument 2)
                                      ,(None))))
          ,(Unlock 1)
          ,(Return (Get-argument 2))))

      ,(Set-pointer "cur" "Node" "val" (Get-argument 2))
      ,(Unlock 1)
      ,(Return (Get-argument 2))))

   (Method
    "get"
    '("Node" "int")
    "int"
    `(,(Lock 1)
      ,(Create-var "cur" "Node")
      ,(Set-var "cur" (Get-argument 0))
      ,(Loop (And (Not (Is-none? (Get-var "cur")))
                  (Not (Equal (Dereference "cur" "Node" "key") (Get-argument 1))))
             `(,(Set-var "cur" (Dereference "cur" "Node" "next"))))
      ,(Single-branch
        (Is-none? (Get-var "cur"))
        `(,(Unlock 1)
          ,(Return (None))))
      ,(Unlock 1)
      ,(Return (Dereference "cur" "Node" "val"))))

   (Method
    "contains"
    '("Node" "int")
    "int"
    `(,(Create-var "val" "int")
      ,(Run-method "get" `(,(Get-argument 0) ,(Get-argument 1)) "val")
      ,(Return (Not (Is-none? (Get-var "val"))))))

   (Method
    "remove"
    '("Node" "int")
    "int"
    `(,(Lock 1)
      ,(Create-var "cur" "Node")
      ,(Create-var "prevNode" "Node")
      ,(Create-var "oldVal" "Node")

      ,(Set-var "cur" (Get-argument 0))
      ,(Single-branch
        (Is-none? (Get-var "cur"))
        `(,(Unlock 1)
          ,(Return  (None))))

      ,(Set-var "oldVal" (Dereference "cur" "Node" "val"))
      ,(Set-var "prevNode" (Get-argument 0))

      ,(Loop (And (Not (Is-none? (Get-var "cur")))
                  (Not (Equal (Dereference "cur" "Node" "key") (Get-argument 1))))
             `(,(Set-var "oldVal" (Dereference "cur" "Node" "val"))
               ,(Set-var "prevNode" (Get-var "cur"))
               ,(Set-var "cur" (Dereference "cur" "Node" "next"))))
      ,(Single-branch
        (Is-none? (Get-var "cur"))
        `(,(Unlock 1)
          ,(Return  (None))))

      ,(Set-var "oldVal" (Dereference "cur" "Node" "val"))
      ,(Set-pointer "prevNode" "Node" "next" (Dereference "cur" "Node" "next"))
      ,(Unlock 1)
      ,(Return (Get-var "oldVal"))))

   (Method
    "extension"
    '("Node" "int")
    "int"
    `(,(Create-var "val" "int")
      ,(Create-var "found" "int")
      ,(Create-var "throwaway" "int")
      ,(Set-var "val" (None))
      ,(Run-method "contains" `(,(Get-argument 0) ,(Get-argument 1)) "found")
      ,(Single-branch
        (Get-var "found")
        `(,(Run-method "get" `(,(Get-argument 0) ,(Get-argument 1)) "val")
          ,(Run-method "remove" `(,(Get-argument 0) ,(Get-argument 1)) "throwaway")))
      ,(Return (Get-var "val"))))))

(bound 1)
(define result
  (error-traces
   library
   "extension"
   (make-hash `(("Node" . (,(Get-var "shared")))))))

(for-each
 (λ (trace)
   (pretty-print (lin-result-trace trace)) (displayln ""))
 result)

(define mooly-test-traces (map lin-result-trace result))
