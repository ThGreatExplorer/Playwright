(
  (module U
    (class Q ()
      (method z () 7.0)))
  (tmodule T
    (timport U (() ((z () Number))))
    (class M ()
      (method run () 0.0))
    (() ((run () Number))))
  (import T)
  (timport U (() ((z () Number))))
  0.0
)
