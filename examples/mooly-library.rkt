#lang racket

(require "../program_representation/simulator-structures.rkt")
(provide
 library)




(define library
  (list

   (Method
    "push"
    (list "Node" "int" "int")
    "int"
    (list
     (Lock 1 )
     (Create-var "cur" "Node" )
     (Create-var "prev" "Node" )
     (Set-var "cur" (Get-argument 0) )
     (Set-var "prev" (Get-argument 0) )
     (Loop (And (Not (Is-none? (Get-var "cur"))) (Not (Equal (Dereference "cur" "Node" "key") (Get-argument 1))))
           (list
            (Set-var "prev" (Get-var "cur") )
            (Set-var "cur" (Dereference "cur" "Node" "next") )))
     (Single-branch
      (Is-none? (Get-var "cur"))
      (list
       (Set-pointer "prev" "Node" "next" (New-struct "Node" (list (None) (Get-argument 1) (Get-argument 2) (None) )) )
       (Unlock 1 )
       (Return (Get-argument 2) )))

     (Set-pointer "cur" "Node" "val" (Get-argument 2) )
     (Unlock 1 )
     (Return (Get-argument 2) )

     ))
     


   (Method
    "get"
    ;; (add-ids add-method-id
    (list "Node" "int")
    "int"
    ;; (create-announcement-version 
     (list
      (Lock 1)
      (Create-var "cur" "Node" )
      (Set-var "cur" (Get-argument 0))
      (Loop  (And (Not (Is-none? (Get-var "cur"))) (Not (Equal (Dereference "cur" "Node" "key") (Get-argument 1))) )
            (list
             (Set-var "cur" (Dereference "cur" "Node" "next") )))
      (Single-branch
        (Is-none? (Get-var "cur"))
       (list
        (Unlock 1)
        (Return (None))))
      (Unlock 1)
      (Return (Dereference "cur" "Node" "val")))
     ;; 0))
    ;; ))
     )
    
    
    (Method
     "contains"

     (list "Node" "int")
     "int"
     ;; (add-ids add-method-id
      (list
       (Create-var "val" "int")
       (Run-method "get" (list (Get-argument 0) (Get-argument 1)) "val") ;; Run method "get" with arguments "key"
       (Return (Not (Is-none? (Get-var "val"))) ))
      ;; 1))
     )
    
   (Method
    "remove"
    (list "Node" "int")
    "int"
    ;; (add-ids add-method-id
     (list
      (Lock 1)
      (Create-var "cur" "Node")
      (Create-var "prevNode" "Node")
      (Create-var "oldVal" "Node")
      
      (Set-var "cur" (Get-argument 0))
      (Single-branch 
                     (Is-none? (Get-var "cur"))
                     (list
                      (Unlock 1)
                      (Return  (None))))
      
      
      (Set-var "oldVal" (Dereference "cur" "Node" "val"))
      (Set-var "prevNode" (Get-argument 0))
     
      (Loop  (And (Not (Is-none? (Get-var "cur"))) (Not (Equal (Dereference "cur" "Node" "key") (Get-argument 1))))

             ;; (And (Not (Equal (Dereference (Dereference "cur" "Node" "next") "Node" "key") (Get-argument 1))) (Not (Equal (Get-var "cur") 0)))
            (list
             (Set-var "oldVal" (Dereference "cur" "Node" "val"))
             (Set-var "prevNode" (Get-var "cur"))           
             (Set-var "cur" (Dereference "cur" "Node" "next"))))

      (Added-CAS-Marker)
      (Single-branch 
       (Is-none? (Get-var "cur"))
       (list
        (Unlock 1)
        (Return  (None))))
      

      (Set-var "oldVal" (Dereference "cur" "Node" "val"))
      (Set-pointer "prevNode" "Node" "next" (Dereference "cur" "Node" "next"))
      (Unlock 1)
      (Return (Get-var "oldVal")))
     ;; 2))
    )


   (Method
    "extension"
    ;; (add-ids add-method-id
    (list "Node" "int")
    "int"
     (list
      (Create-var "val" "int" )
      (Create-var "found" "int" )
      (Create-var "throwaway" "int")
      ;; (Label "START")
      (Set-var "val" (None) )
      (Run-method "contains" (list (Get-argument 0) (Get-argument 1)) "found")
      (Single-branch 
       (Get-var "found") 
       (list
        (Run-method "get" (list (Get-argument 0) (Get-argument 1)) "val")
        (Run-method "remove" (list (Get-argument 0) (Get-argument 1)) "throwaway")))
      (Return (Get-var "val")))
    )))
     
     ;; 3))))

    
