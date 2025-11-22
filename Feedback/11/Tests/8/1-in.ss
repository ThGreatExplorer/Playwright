(
  (module A (class A ()))
  (module B (import A) (class B ()))
  (module C (import A) (import B) (class C ()))
  (tmodule D (timport A (() ())) (timport B (() ())) (timport C (() ())) (class D ()) (() ()))

  (timport A (() ()))
  (timport B (() ()))
  (timport C (() ()))
  (import D)
  
  1.0
)