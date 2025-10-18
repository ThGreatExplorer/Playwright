((class C ()
   (method id (x) x))
 (def a 1.0)
 (if0 a
   (block (def y a) (a = y))
   (a = y))
 a)