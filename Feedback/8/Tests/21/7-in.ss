(
  (module P (class C () (method id () 9.5)))
  (import P)
  (def o (new C ()))
  (o --> id ())
)
