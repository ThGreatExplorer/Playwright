((module M
   (class C ()
     (method returnTen () 10.0)))

 (module K
   (class C ()
     (method returnNine () (def zero 0.0) (zero / zero))))

 (timport K (() ((returnNine () Number))))
 (def c (new C ()))
 (c --> returnNine ()))