((tmodule
  Box
  (class Box (value) (method get () (this --> value)) (method set (v) (this --> value = v) v))
  (((value Number)) ((get () Number) (set (Number) Number))))
 (import Box)
 (def x 10.0)
 (def box (new Box (x)))
 (def y 20.0)
 (box --> set (y)))
