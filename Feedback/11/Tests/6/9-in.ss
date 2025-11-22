(
  (module U
    (class Z ()
      (method k () 4.0)))
  (tmodule Tone
    (timport U (() ((k () Number))))
    (class A ()
      (method f () 0.0))
    (() ((f () Number))))
  (tmodule Ttwo
    (timport U (() ((f () Number))))
    (class B ()
      (method g () 0.0))
    (() ((g () Number))))
  (import Ttwo)
  0.0
)