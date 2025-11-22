(
  (module A
    (class x ()
      (method n () 0.0)))
  (module B
    (class y ()
      (method m () 0.0)))
  (tmodule T
    (timport A (() ((n () Number))))
    (timport B (() ((m () Number))))
    (class Main ()
      (method run () 0.0))
    (() ((run () Number))))
  (import T)
  0.0
)
