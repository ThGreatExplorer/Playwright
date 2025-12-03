((module U
  (class U
    (x)
    (method get ()
      (def tmp (this --> x))
      tmp)))
 (tmodule
  BadImport
  (import U)
  (class BadImport
    (dummy)
    (method run ()
      (def tmp (this --> dummy))
      tmp))
  (((dummy Number)) ((run () Number))))
 (def z 0.0)
 z)

