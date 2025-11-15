((module U (class U (x) (method id () x)))
 (tmodule
   T
   (timport U (((x Number)) ((id () Number))))
   (timport U (((x Number)) ((id (Number) Number))))
   (class T () (method m () 0.0))
   (() ((m () Number))))
 0.0)
