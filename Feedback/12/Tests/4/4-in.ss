(
  (tmodule M
    (class C ()
      (method id () 1.0))
    ( () ( (id () Number) ) ) )

  (import M)
  (def c (new C ()))
  (c --> id ())
)