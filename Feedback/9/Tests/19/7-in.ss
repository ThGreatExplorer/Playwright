(
  (tmodule MOne
    (class C ()
      (method val () 1.0))
    (() ((val () Number))))
  (tmodule MTwo
    (class C ()
      (method val () 2.0))
    (() ((val () Number))))
  (import MOne)
  (import MTwo)
  (def o (new C ()))
  (o --> val ())
)
