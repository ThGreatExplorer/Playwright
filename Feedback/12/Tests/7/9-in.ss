(
  (module A (class A ()))
  (module B (import A) (class B ()))
  (module C (import B) (class C () (method myMethod (a) (new B ()))))

  (tmodule D (timport C (() ((myMethod (Number) Number)))) 
    (class D () (method myMethodTwo (a) (def one 1.0) (def obj (new C ())) (obj --> myMethod (one))))
    (() ((myMethodTwo (Number) Number))))
  (import D)
  (def a (new D ()))
  (def one 1.0)
  (def out (a --> myMethodTwo (one)))
  1.0
)