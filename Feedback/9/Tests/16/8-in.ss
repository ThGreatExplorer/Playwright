((tmodule M
   (class C ()
     (method add (x y) (x + y)))
   (() ((add (Number Number) Number))))
 (import M)
 (def c (new C ()))
 (c --> add ()))
