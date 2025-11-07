((tmodule
  Point
  (class Point (x y) (method delta (x) (def y (this --> y)) (x = 1.0) (x + y)))
  (((x Number) (y Number)) ((delta (Number) Number)))
  )
 (tmodule
  PointTwo
  (import Point)
  (class PointT
    (x y z)
    (method delta () (def x (this --> x)) (def y (this --> y)) (def p (new Point (x y)) ) p))
  (((x Number) (y Number) (z Number))
   ((delta () 
     (((x Number) (y Number)) ((delta (Number) Number)))
   ))
   ))
 (import PointTwo)
 (def x 1.0)
(def y 1.0)
(def point (new PointT (x x x)))
 (x = (point --> delta ()))
 (x = (x --> delta (y)))
 x)
