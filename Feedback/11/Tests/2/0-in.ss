((module A
  (class A ()
   (method m ()
    (def x 1.0)
    x)))
 (timport A (() ((m () Number))))
 (def o (new A ()))
 (o --> m ()))
