((tmodule M
   (class C ()
     (method id (x) x))
   (() ((id (Number) Number))))
 (import M)
 (def c (new C ()))
 (c --> id (c)))