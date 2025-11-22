((tmodule
  Cowboy
  (class Cowboy () (method draw () 1.0))
  (() ((draw () Number))))
 (module into (class Artist () (method draw () 666.0)))
 (import Cowboy)
 (timport into (() ((draw () Number))))
 (def a (new Artist ()))
 (def c (new Cowboy ()))
 (def what 1.0)
 (def x (new Artist ()))
 (if0 what (x = a) (x = c))
 (x --> draw ()))