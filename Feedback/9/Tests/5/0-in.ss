((tmodule
  Simple
  (class Simple () (method getSum (x y) (x + y)))
  (() ((getSum (Number Number) Number))))
 (import Simple)
 (def a 5.5)
 (def b 4.5)
 (def s (new Simple ()))
 (s --> getSum (a b)))