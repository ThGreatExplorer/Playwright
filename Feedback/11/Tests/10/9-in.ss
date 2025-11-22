((tmodule
  TypedA
  (class A (x) (method getX () (this --> x)))
  (((x Number)) ((getX () Number))))
 (tmodule
  TypedB
  (import TypedA)
  (class B (a) (method getValue () (def obj (this --> a)) (obj --> getX)))
  (((a (((x Number)) ((getX () Number))))) ((getValue () Number))))
 (import TypedA)
 (import TypedB)
 (def n 15.0)
 (def a (new A (n)))
 (def b (new B (a)))
 (b --> getValue))
