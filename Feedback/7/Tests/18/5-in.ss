(
  (class C
    (x y)
    (method delta (x)
      (def a 1000.0)
      (def b (this --> x))
      (this --> x = a)
      (b = (a + a))
      b))
  (def b 5.0)
  (def c 10.0)
  (def classVar (new C (b c)))
  (def result (classVar --> delta (c)))
  result
)
