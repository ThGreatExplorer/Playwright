((tmodule M
   (class C ()
     (method make () 0.0))
   (() ((make () Number))))
 (import M)
 (def c (new C ()))
 c)