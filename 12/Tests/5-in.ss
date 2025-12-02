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
 (tmodule 
 PointDelegater
 (class PointDelegater (a))
 (((a (((y Number) (x Number)) ((delta (Number) Number))))) ())
 )
 (module
  PointCaller
  (import Point)
  (import PointB)
  (import PointDelegater)
  (class PointCaller () 
  (method reassign () 
    (def x 1.0)
    (def point (new Point (x x)))
    (def pointB (new PointB (x x)))
    (def pointCaller (new PointDelegater (pointB)))
    (pointCaller --> a = point)
    (pointCaller --> a)))
 )
 (import PointB)
 (timport PointCaller (() ((reassign () (((y Number) (x Number)) ((delta (Number) Number)))))))
 (def x 1.0)
 (def pointB (new PointB (x x)))
 (def pointCaller (new PointCaller ()))
 x
 )