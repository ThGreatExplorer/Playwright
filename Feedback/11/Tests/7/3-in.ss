 ((tmodule
      Cowboy
      (class Cowboy () (method draw () 1.0))
      (() ((draw () Number))))
     (tmodule
      Artist
      (class Artist () (method draw () 666.0))
      (() ((draw () Number))))
     (import Cowboy)
     (import Artist)
     (def a (new Artist ()))
     (def c (new Cowboy ()))
     (def what 1.0)
     (def x (new Artist ()))
     (if0 what (x = a) (x = c))
     (x --> draw ()))