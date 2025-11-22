((module U
   (class U ()
     (method m () 0.0)))
 (tmodule K
   (import U)
   (class K ()
     (method k () 1.0))
   (() ((k () Number))))
 0.0)
