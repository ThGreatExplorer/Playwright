((tmodule M (class D () (method m () 666.0)) (() ((m () Number))))
 (tmodule
  N
  (import M)
  (class C () (method id () 1.0) (method m () (new D ())))
  (() ((id () Number) (m () (() ((m () Number)))))))
 (import M)
 (import N)
 (def c (new C ()))
 (def d (c --> m ()))
 (def e (d --> m ()))
 (def f (c --> id ()))
 (e + f))
