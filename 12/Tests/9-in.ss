((tmodule
  Point
  (class Point (x y) (method delta (x) (def y (this --> y)) x))
  (((x Number) (y Number)) ((delta (Number) Number)))
  )
 (tmodule 
  PointB
  (class PointB (y x) (method delta (x) x))
  (((y Number) (x Number)) ((delta (Number) Number)))
 )
 (module
  PointCaller
  (import Point)
  (import PointB)
  (import Point)
  (class PointCaller (x) 
  (method reassign () 
    (def x 1.0)
    (def point (new Point (x x)))
    (def pointB (new PointB (x x)))
    (def pointCaller (new PointCaller (pointB)))
    (pointCaller --> x = point)
    (pointCaller --> x)))
 )
 (import PointB)
 (timport PointCaller (((x (((y Number) (x Number)) ((delta (Number) Number))))) ((reassign () (((y Number) (x Number)) ((delta (Number) Number)))))))
  (timport PointCaller (((x (((y Number) (x Number)) ((delta (Number) Number))))) ((reassign () (((y Number) (x Number)) ((delta (Number) Number)))))))
 (def x 1.0)
 (def pointB (new PointB (x x)))
 (def pointCaller (new PointCaller (pointB)))
 x
 )