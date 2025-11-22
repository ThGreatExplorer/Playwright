((module A (class B ()))
(tmodule X
  (timport A (() ()))
  (class Body ())
  (() ()))

  (timport A (() ()))
  (import X)
  1.0)