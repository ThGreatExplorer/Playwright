((module U
   (class C () (method m () 0.0)))
 (tmodule T
   (import U) (class D () (method d () 0.0))
   (() ((d () Number))))
 0.0)
