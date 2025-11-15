((tmodule M
   (class C () (method m () 0.0))
   (() ((m () Number))))
 (tmodule T
   (timport M (() ((m () Number))))
   (class D () (method d () 0.0))
   (() ((d () Number))))
 0.0)
