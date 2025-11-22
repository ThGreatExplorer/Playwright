((tmodule
  PointThreeD
  (class PointThreeD
    (x y z)
    (method delta () (def x (this --> x)) (def y (this --> y)) (x + y)))
  (((x Number) (z Number) (y Number)) ((delta (Number) Number))))
 (import PointThreeD)
 (def x 1.0)
 (def point (new PointThreeD (x x x)))
 (point --> x = x)
 (x = (point --> delta ()))
 x)
