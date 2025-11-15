((module Mone (class cone () (method m (x) x)))
 (tmodule
  Mtwo
  (timport Mone (() ((m (Number) Number))))
  (class ctwo () (method m (x) x))
  (() ((m ((() ((m (Number) Number)))) (() ((m (Number) Number)))))))
 (timport Mone (() ((m (Number) Number))))
 (import Mtwo)
 (def mone (new cone ()))
 (def mtwo (new ctwo ()))
 (def monetoo (mtwo --> m (mone)))
 (def two 2.0)
 (monetoo --> m (two)))
