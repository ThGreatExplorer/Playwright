(
  (tmodule
    A
    (class A
      (f)
      (method inc (x)
        (def v (this --> f))
        (x + v)))
    (
      ((f Number))
      ((inc (Number) Number))
    ))

  (tmodule
    B
    (class B
      (a)
      (method run (y)
        (def aobj (this --> a))
        (def z (aobj --> inc (y)))
        (z + y)))
    (
      ((a
        (
          ((f Number))
          ((inc (Number) Number))
        )))
      ((run (Number) Number))
    ))

  (import A)
  (import B)

  (def two 2.0)
  (def three 3.0)

  (def a (new A (two)))
  (def b (new B (a)))

  (def cond 0.0)
  (while0 cond (block (cond = three)))

  (b --> run (three))
)