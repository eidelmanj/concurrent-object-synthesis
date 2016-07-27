; Library, including extension
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
                       (New-struct "Node" `(,(None) ,(Get-argument 1)
                                            ,(Get-argument 2) ,(None))))
         ,(Unlock 1)
         ,(Return (Get-argument 2))))

    ,(Set-pointer "cur" "Node" "val" (Get-argument 2))
    ,(Unlock 1 )
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
         ,(Return (None))))

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
         ,(Return (None))))

    ,(Set-var "oldVal" (Dereference "cur" "Node" "val"))
    ,(Set-pointer "prevNode" "Node" "next" (Dereference "cur" "Node" "next"))
    ,(Unlock 1)
    ,(Return (Get-var "oldVal"))))

 (Method
  "extension"
  '("Node" "int")
  "int"
  `(,(Create-var "val" "int" )
    ,(Create-var "found" "int" )
    ,(Create-var "throwaway" "int")
    ,(Set-var "val" (None) )
    ,(Run-method "contains" `(,(Get-argument 0) ,(Get-argument 1)) "found")
    ,(Single-branch
      (Get-var "found")
      `(,(Run-method "get" `(,(Get-argument 0) ,(Get-argument 1)) "val")
        ,(Run-method "remove" `(,(Get-argument 0) ,(Get-argument 1)) "throwaway")))
    ,(Return (Get-var "val")))))

; Error trace 1
(list
 (Create-var '() 0 "val" "int")
 (Create-var '() 1 "found" "int")
 (Create-var '() 2 "throwaway" "int")
 (Set-var '() 3 "val" (None))
 (Run-method
  '()
  4
  "contains"
  (list (Get-argument '() '() 0) (Get-argument '() '() 1))
  "found")
 (Assume-simulation '() '() (Get-var "found"))
 (Run-method
  '()
  5
  "get"
  (list (Get-argument '() '() 0) (Get-argument '() '() 1))
  "val")
 (Run-method #t '() "push" (list (Get-var "shared") 1 1) 'push5-1)
 (Run-method
  '()
  6
  "remove"
  (list (Get-argument '() '() 0) (Get-argument '() '() 1))
  "throwaway")
 (Run-method #t '() "remove" (list (Get-var "shared") 1) 'remove6-1)
 (Return '() 7 (Get-var "val")))

; Error trace 2
(list
 (Create-var '() 0 "val" "int")
 (Create-var '() 1 "found" "int")
 (Create-var '() 2 "throwaway" "int")
 (Set-var '() 3 "val" (None))
 (Run-method
  '()
  4
  "contains"
  (list (Get-argument '() '() 0) (Get-argument '() '() 1))
  "found")
 (Assume-simulation '() '() (Get-var "found"))
 (Run-method
  '()
  5
  "get"
  (list (Get-argument '() '() 0) (Get-argument '() '() 1))
  "val")
 (Run-method #t '() "push" (list (Get-var "shared") 1 1) 'push5-1)
 (Run-method
  '()
  6
  "remove"
  (list (Get-argument '() '() 0) (Get-argument '() '() 1))
  "throwaway")
 (Run-method #t '() "contains" (list (Get-var "shared") 1) 'contains6-1)
 (Return '() 7 (Get-var "val")))

; Error trace 3
(list
 (Create-var '() 0 "val" "int")
 (Create-var '() 1 "found" "int")
 (Create-var '() 2 "throwaway" "int")
 (Set-var '() 3 "val" (None))
 (Run-method
  '()
  4
  "contains"
  (list (Get-argument '() '() 0) (Get-argument '() '() 1))
  "found")
 (Assume-simulation '() '() (Get-var "found"))
 (Run-method
  '()
  5
  "get"
  (list (Get-argument '() '() 0) (Get-argument '() '() 1))
  "val")
 (Run-method #t '() "push" (list (Get-var "shared") 1 1) 'push5-1)
 (Run-method
  '()
  6
  "remove"
  (list (Get-argument '() '() 0) (Get-argument '() '() 1))
  "throwaway")
 (Run-method #t '() "get" (list (Get-var "shared") 1) 'get6-1)
 (Return '() 7 (Get-var "val")))

; Error trace 4
(list
 (Create-var '() 0 "val" "int")
 (Create-var '() 1 "found" "int")
 (Create-var '() 2 "throwaway" "int")
 (Set-var '() 3 "val" (None))
 (Run-method
  '()
  4
  "contains"
  (list (Get-argument '() '() 0) (Get-argument '() '() 1))
  "found")
 (Assume-simulation '() '() (Get-var "found"))
 (Run-method
  '()
  5
  "get"
  (list (Get-argument '() '() 0) (Get-argument '() '() 1))
  "val")
 (Run-method #t '() "remove" (list (Get-var "shared") 1) 'remove5-1)
 (Run-method
  '()
  6
  "remove"
  (list (Get-argument '() '() 0) (Get-argument '() '() 1))
  "throwaway")
 (Return '() 7 (Get-var "val")))

; Minimal covers
'((6 . "get") (5 . "remove") (6 . "remove") (6 . "contains"))
'((5 . "remove") (5 . "push"))