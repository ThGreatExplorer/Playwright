(
  (module A (class A ()))
  (module B (import A) (class B ()))
  (module C (import B) (class C ()))
  (module D (class D ()))
  (tmodule E (timport D (() ())) (class E ()) (() ()))
  (tmodule F (timport D (() ())) (import E) (class F ()) (() ()))
  1.0
)