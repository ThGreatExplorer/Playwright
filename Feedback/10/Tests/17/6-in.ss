((module U
   (class C () (method m () 0.0)))
 (tmodule T
   (timport U (() ((m () Number))))
   (timport U (() ((m (Number) Number)))) ; different signature
   (class D () (method d () 0.0))
   (() ((d () Number))))
 0.0)
