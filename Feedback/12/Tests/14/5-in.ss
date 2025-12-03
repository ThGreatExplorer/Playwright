(
  (module A
    (class C ()
      (method m () (new C ()))))
  (timport A (() ((m () Number))))
  (def x (new C ()))
  (x --> m ())
)