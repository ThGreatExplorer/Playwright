((module M
  (class First
    ()
    (method foo () 0.0)))
 (tmodule
  M
  (class Second
    ()
    (method bar () 1.0))
  (() ((bar () Number))))
 (def x 0.0)
 x)