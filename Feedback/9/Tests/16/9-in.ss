((tmodule M
   (class C ()
     (method m () 0.0))
   (() ((m () Number))))
 (tmodule B
   (import M)
   (class Box (x)
     (method get () (this --> x)))
   (((x Number)) ((get () Number))))
 (import B)
 (def o (new C ()))
 (def b (new Box (o)))
 0.0)