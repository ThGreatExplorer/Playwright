((module
  Point
  (class Point (x y) (method delta (x) (def y (this --> y)) x)))
 (module 
  PointB
  (class PointB (y x) (method delta (x) x))
 )
 (tmodule
  PointCaller
  (class PointCaller (x))
  (((x (((y Number) (x Number)) ((delta (Number) Number))))) ())
 )
 (timport Point (((x Number) (y Number)) ((delta (Number) Number))))
 (timport PointB (((y Number) (x Number)) ((delta (Number) Number))))
 (import PointCaller)
 (def x 1.0)
 (def point (new Point (x x)))
 (def pointB (new PointB (x x)))
 (point isa PointB)
 )