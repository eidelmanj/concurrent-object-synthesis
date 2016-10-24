#lang racket

(require "../program_representation/simulator-structures.rkt")
(provide
 map-library)




(define map-library
  (list

   (Method
    "push"
    (list "List" "int" "int")
    "int"
    (list
     (Lock 1 )
     (Create-var "cur" "Node" )
     (Create-var "prev" "Node" )
     (Create-var "l1" "List")
     (Set-var "l1" (Get-argument 0))



     
     
     (Set-var "cur" (Dereference "l1" "List" "first"))


     (Single-branch (Is-none? (Get-var "cur"))
                    (list
                     (Set-var "cur" (New-struct "Node" (list (None) (Get-argument 1) (Get-argument 2) (None))))
                     (Set-pointer "l1" "List" "first" (Get-var "cur"))))
     
     (Set-var "prev" (Dereference "l1" "List" "first"))

     
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
    (list "List" "int")
    "int"
    ;; (create-announcement-version 
     (list
      (Lock 1)
      (Create-var "l1" "List")
      
      (Create-var "cur" "Node" )
      (Set-var "l1" (Get-argument 0))
      (Set-var "cur" (Dereference "l1" "List" "first"))
      
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


   ;; (Method
   ;;   "putIfAbsent"

   ;;   (list "List" "int" "int")
   ;;   "int"
   ;;   ;; (add-ids add-method-id
   ;;    (list
   ;;     (Create-var "check" "int")
   ;;     (Create-var "val" "int")
   ;;     (Run-method "get" (list (Get-argument 0) (Get-argument 1)) "check")

   ;;     (Single-branch
   ;;      (Is-none? (Get-var "check"))
   ;;      (list
   ;;       (Run-method "push" (list (Get-argument 0) (Get-argument 1) (Get-argument 2)) "val")
   ;;       (Return (Get-var "val"))))

   ;;     (Return (None))))


         

                      
       
   ;;    ;; 1))


    
    
   ;;  (Method
   ;;   "contains"

   ;;   (list "List" "int")
   ;;   "int"
   ;;   ;; (add-ids add-method-id
   ;;    (list
   ;;     (Create-var "val" "int")
   ;;     (Run-method "get" (list (Get-argument 0) (Get-argument 1)) "val") ;; Run method "get" with arguments "key"
   ;;     (Return (Not (Is-none? (Get-var "val"))) ))
   ;;    ;; 1))
   ;;   )


   ;;  (Method
   ;;   "compute"
   ;;   (list "int")
   ;;   "int"

   ;;   (list
   ;;    (Return (Multiply (Get-argument 0) 2))))
    
   (Method
    "remove"
    (list "List" "int")
    "int"
    ;; (add-ids add-method-id
     (list
      (Lock 1)
      (Create-var "l1" "List")

      (Set-var "l1" (Get-argument 0))
      (Create-var "cur" "Node")
      (Create-var "prevNode" "Node")
      (Create-var "oldVal" "Node")
      
      (Set-var "cur" (Dereference "l1" "List" "first"))
      (Single-branch 
                     (Is-none? (Get-var "cur"))
                     (list
                      (Unlock 1)
                      (Return  (None))))


      (Single-branch
       (Is-none? (Dereference "cur" "Node" "next"))
       (list
        (Set-pointer "l1" "List" "first" (None))
        (Unlock 1)
        (Return (Dereference "cur" "Node" "val"))
        ))
      
      
      (Set-var "oldVal" (Dereference "cur" "Node" "val"))
      (Set-var "prevNode" (Dereference "l1" "List" "first"))
     
      (Loop  (And (Not (Is-none? (Get-var "cur"))) (Not (Equal (Dereference "cur" "Node" "key") (Get-argument 1))))

             ;; (And (Not (Equal (Dereference (Dereference "cur" "Node" "next") "Node" "key") (Get-argument 1))) (Not (Equal (Get-var "cur") 0)))
            (list
             (Set-var "oldVal" (Dereference "cur" "Node" "val"))
             (Set-var "prevNode" (Get-var "cur"))           
             (Set-var "cur" (Dereference "cur" "Node" "next"))))

      ;; (Added-CAS-Marker)
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
    )))


   ;; (Method
   ;;  "extension"
   ;;  ;; (add-ids add-method-id
   ;;  (list "Node" "int")
   ;;  "int"
   ;;   (list
   ;;    (Create-var "val" "int" )
   ;;    (Create-var "found" "int" )
   ;;    (Create-var "throwaway" "int")
   ;;    ;; (Label "START")
   ;;    (Set-var "val" (None) )
   ;;    (Run-method "contains" (list (Get-argument 0) (Get-argument 1)) "found")
   ;;    (Single-branch 
   ;;     (Get-var "found") 
   ;;     (list
   ;;      (Run-method "get" (list (Get-argument 0) (Get-argument 1)) "val")
   ;;      (Run-method "remove" (list (Get-argument 0) (Get-argument 1)) "throwaway")))
   ;;    (Return (Get-var "val")))
   ;;  )))
     
     ;; 3))))

    
