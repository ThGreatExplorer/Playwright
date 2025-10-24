(
  (class C
   (x y)
   (method delta (x) (def z (this --> x)) (def x (x + x)) (z + x)))
   (def b 5.0)
   (def c 10.0)
   (def classVar (new C (b c)))
   (classVar --> x)
 )