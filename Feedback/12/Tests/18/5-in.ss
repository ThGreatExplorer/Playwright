((tmodule T
   (class C ()
     (method m ()
       (new C ())))
   (() ((m () Number))))
 (import T)
 (def o (new C ()))
 (o --> m ()))
