((module M
   (class C ()
     (method returnTen () 10.0)))

 (module K
   (class C ()
     (method returnNine () 9.0)))

 (import K)
 (def c (new C ()))
 (c --> returnTen ()))
