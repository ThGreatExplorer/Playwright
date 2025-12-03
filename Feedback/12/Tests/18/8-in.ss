((tmodule T
   (class C ()
     (method m ()
       5.0))
   (() ((m () Number))))
 (import T)
 (def o (new C ()))
 (o --> m ()))
