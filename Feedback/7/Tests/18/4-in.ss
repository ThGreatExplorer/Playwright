(
  (class C
   (x y)
   (method delta (x) (def z (this --> x)) (def x (x + x)) (z + x)))
   (class B
   (g h))
   (def b 5.0)
   (def c 10.0)
   (def d 5.0)
   (def classVar (new C (b c)))
   (def classVarTwo (new C (d c)))
   (classVar == classVarTwo)
 )