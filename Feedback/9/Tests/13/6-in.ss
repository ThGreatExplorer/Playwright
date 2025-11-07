((tmodule
  Point
  (class Point (x y) (method delta (x) (def y (this --> y)) 
  (def s 1.0)
  (def t 2.0) 
  (def clas (new Point (s t))) (x = 1.0) (x + y)))
  (((x Number) (y Number)) ((delta (Number) Number))))
  
 (tmodule
  PointTwo
  (class Point
    (x y z)
    (method delta (x) (def x (this --> x)) (def y (this --> y)) (x + y)))
  (((x Number) (y Number) (z Number))
   ((delta (Number) Number))))

 (import Point)
 (import PointTwo)

 
 (def x 1.0)
 (def point (new Point (x x x)))

 x)
