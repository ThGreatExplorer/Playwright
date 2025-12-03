(
  (module A (class A ()))
  (module B (import A) (class B ()))
  (module C (import A) (import B) (class C ()  (method myMethod (x) 1.0)))
  (tmodule D (timport C (() ((myMethod (Number) Number)))) (class D (f)) (((f Number)) ()))
  
  (import D)
  (def one 1.0)
  (def a (new D (one)))
  (def c (new D (a)))
  1.0
)