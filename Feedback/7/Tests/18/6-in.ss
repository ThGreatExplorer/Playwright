(
  (class C
    (x y)
    (method delta (x)
      (def a 1000.0)
      (def b (this --> x))
      (this --> x = a)
      (b = (a + a))
      b))
  (class B
    (x y)
    (method delta (x)
    (def a -1.0)
    (def b 2.0)
      (def prevClass (new C (a b)))
      (a = (prevClass --> x))
      (a / x)
      ))
  (def b 5.0)
  (def c 10.0)
  (def classVar (new B (b c)))
  (def result (classVar --> delta (c)))
  result
)
