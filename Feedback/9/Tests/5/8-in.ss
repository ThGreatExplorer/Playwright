((tmodule
  Divider
  (class Divider () (method div (x y) (x / y)))
  (() ((div (Number Number) Number))))
 (import Divider)
 (def d (new Divider ()))
 (def vone 5.0)
 (def vtwo 0.0)
 (d --> div (vone vtwo)))