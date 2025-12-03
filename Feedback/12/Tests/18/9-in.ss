((module A
   (class C ()
     (method id () this)))
 (timport A (() ()))
 (def one 1.0)
 (def c (new C ()))
 (one + c))
