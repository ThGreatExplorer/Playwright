((tmodule M
   (class C ()
     (method add (x y) (x + y)))
   (() ((add (Number Number) Number))))
 (import M)
 (def a 1.0)
 (def b 2.0)
 (def o (new C ()))
 (o --> add (a b)))