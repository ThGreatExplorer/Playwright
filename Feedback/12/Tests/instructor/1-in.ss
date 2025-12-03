((module Mone (class cone () (method m (x) x)))
 (tmodule
  Mtwo
  (timport Mone (() ((m (Number) Number))))
  (class ctwo () (method m (x) (def c (new cone ())) (c --> m (x))))
  (() ((m (Number) Number))))
 (import Mtwo)
 (def mone (new ctwo ()))
 (def two 2.0)
 (mone --> m (two)))
