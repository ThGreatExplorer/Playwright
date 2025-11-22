((module Mone (class cone () (method m (x) x)))
 (tmodule
  Mtwo
  (import Mone)
  (class ctwo () (method m (x) x))
  (() ((m (Number) Number))))
 (timport Mone (() ((m (Number) Number))))
 (def mone (new Mone ()))
 (def two 2.0)
 (mone --> m (two)))
