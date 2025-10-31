((module A
    (class A
      (x)
      (method f () x)))
 (import A)
 (def one 1.0)
 (def b (new A (one)))
 (def c (b --> f ()))
 (x + y))
