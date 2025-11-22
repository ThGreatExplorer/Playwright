((module A (class B ()))
(tmodule X
  (timport A (() ()))
  (class Y ())
  (() ()))

  (timport A (() ()))
  (timport X (() ()))
  1.0)