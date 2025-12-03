(
  (module B
    (class B ()
      (method m () 1.0)))

  (module A
    (import B)
    (class C ()
      (method make () (new B ()))))

  (timport A
    ( ()
      ((make () (() ())))))

  (def x (new C ()))
  (def y (x --> make ()))

  1.0
)
