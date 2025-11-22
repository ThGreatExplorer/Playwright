(
  (module U
    (class C ()
      (method m () 0.0)))

  (tmodule T
    (import U)
    (class D ()
      (method run ()
        0.0))
    (() ((run () Number))))

  0.0
)
