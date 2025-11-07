((tmodule
  A
  (class A () (method f () 1.0))
  (() ((f () Number))))
 (tmodule
  A
  (class B () (method g () 2.0))
  (() ((g () Number))))
 (import A)
 (def x 1.0)
 x)