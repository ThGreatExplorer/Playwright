((tmodule M
   (class C ()
     (method m () 0.0))
   (() ((m () Number))))
 (import M)
 (def o (new C ()))
 (o --> m ()))