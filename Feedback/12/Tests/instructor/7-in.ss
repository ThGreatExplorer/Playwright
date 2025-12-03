((module Mone (class cone () (method m (x) x)))
 (tmodule
  Mtwo
  (timport Mone (() ((m ((() ((m (Number) Number)))) Number))))
  (class ctwo () (method m (y) (def o 1.0) (y --> m (o))))
  (() ((m ((() ((m (Number) Number)))) Number))))
 (timport Mone (() ((m (Number) Number))))
 (import Mtwo)
 (def x (new ctwo ()))
 (def y (new cone ()))
 (def two 2.0)
 (x --> m (y)))
