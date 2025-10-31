(
  (module P (class C () (method id () 6.0)))
  (import P)
  (def o (new C ()))
  (o --> id ())
)
