((tmodule M 
   (class C (x y)) 
   (((x Number) (y Number))
    ()))
 (import M)
 (def one 1.0)
 (new C (one)))