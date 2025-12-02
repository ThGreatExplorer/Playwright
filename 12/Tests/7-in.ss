((module
  Point
  (class Point (x y) (method delta (x) (def y (this --> y)) x)))
 (tmodule
  PointB
  (class Point (x y) (method delta (x) (def y (this --> y)) x))
  (((x Number) (y Number)) ((delta (Number) Number)))
 )
 (timport Point (((x Number) (y Number)) ((delta (Number) Number))))
 (import PointB)
 (def x 1.0)
 (def point (new Point (x x)))
 (point isa Point)
)
 
