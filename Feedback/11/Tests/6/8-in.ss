(
  (module U
    (class X ()
      (method n () 9.0)))
  (module M
    (class C ()
      (method f () 0.0)))
  (timport U (() ((n () Number))))
  0.0
)