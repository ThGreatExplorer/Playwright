((tmodule
  Point
  (class Point (x y) (method delta (x) (def y (this --> y)) (x = 1.0) (x + y)))
  (((x Number) (y Number)) ((delta (Number) Number))))
  
 (tmodule
  PointTwo
  (class Point
    (x y z)
    (method delta () (def x (this --> x)) (def y (this --> y)) (x + y)))
  (((x Number) (y Number) (z Number))
   ((delta () Number))))

 (import PointTwo)
 (import Point)

 
 (def x 1.0)
 (def point (new Point (x x)))
 (point --> x = x)
 (x = (point --> delta (x)))
 x)
