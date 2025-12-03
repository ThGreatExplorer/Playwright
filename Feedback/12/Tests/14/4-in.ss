(
  (module A
    (class C ()
      (method m () 5.0)))
  (timport A (() ((m () Number))))
  (def x (new C ()))
  (x --> m ())
)