((tmodule P
  (class P () (method m () 1.0))
  (() ((m () Number))))
(import P)
(def x (new P ()))
x)
