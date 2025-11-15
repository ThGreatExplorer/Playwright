(
  (tmodule N
    (class C ()
      (method go ()
        (def a 1.0)
        (def b 0.0)
        (a / b)
      ))
    ( () ((go () Number)) ))

  (import N)
  (def y (new C ()))
  (y --> go ())
)
