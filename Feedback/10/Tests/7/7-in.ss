((tmodule
  Box
  (class Box (n) (method double () (def x (this --> n)) (x + x)))
  (((n Number)) ((double () Number))))
 (import Box)
 (def val 9.0)
 (def box (new Box (val)))
 (box --> double ()))