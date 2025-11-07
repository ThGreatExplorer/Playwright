((tmodule
  M
  (class M () (method f () 1.0))
  (() ((f () Number))))
 (import M)
 (def y (new M ()))
 (x = 5.0)
 (y --> f))