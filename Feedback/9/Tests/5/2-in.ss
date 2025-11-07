((tmodule
  BadMethods
  (class BadMethods ()
    (method m () 1.0)
    (method m (x) (def one 1.0) (x + one)))
  (() ((m () Number) (m (Number) Number))))
 (import BadMethods)
 (def d 1.0)
 d)