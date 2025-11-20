(
  (module A (class A ()))
  (module B (class B ()))
  (module C (class C ()))
  (module D (class D ()))
  (tmodule E (timport D (() ())) (class E ()) (() ()))
  (tmodule F (timport D (() ())) (class F ()) (() ()))
  (timport D (() ()))
  (timport D (() ()))
  (timport D (() ()))
  1.0
)