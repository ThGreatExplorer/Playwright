(
  (module U
    (class C ()
      (method k () 1.0)))
  (tmodule Tone
    (timport U (() ((k () Number))))
    (class A ()
      (method f () 0.0))
    (() ((f () Number))))
  (tmodule Ttwo
    (timport U (() ((k () Number))))
    (class B ()
      (method g () 0.0))
    (() ((g () Number))))
  (import Tone)
  (import Ttwo)
  0.0
)