(
  (module A (class C ()))
  (module B (class D ()))
  (import A)
  (import B)
  (def o (new D ()))
  (o isa C)
)
