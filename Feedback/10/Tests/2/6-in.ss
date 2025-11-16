((module U
  (class U
    (x)
    (method get ()
      (def tmp (this --> x))
      tmp)))
 (tmodule
  Main
  (class Main
    ()
    (method dummy () 0.0))
  (() ((dummy () Number))))
 (import U)
 (def a 1.0)
 a)