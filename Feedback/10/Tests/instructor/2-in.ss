((tmodule Mone (class cone () (method m (x) x)) (() ((m (Number) Number))))
 (tmodule
  Mtwo
  (timport
   Mone
   (() ((m ((() ((m (Number) Number)))) (() ((m (Number) Number)))))))
  (class ctwo () (method m (x) x))
  (() ((m (Number) Number))))
 (timport Mone (() ((m (Number) Number))))
 (def mone (new Mone ()))
 (def two 2.0)
 (mone --> m (two)))
