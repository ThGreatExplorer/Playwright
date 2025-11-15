((tmodule
  A
  (class A
    ()
    (method id (x) x))
  (() ((id (Number) Number))))
 (tmodule
  B
  (timport A (() ((id (Number) Number))))
  (class B
    ()
    (method use ()
      (def one 1.0)
      one))
  (() ((use () Number))))
 (def y 0.0)
 y)
