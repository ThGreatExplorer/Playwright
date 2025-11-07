((tmodule
  Point
  (class Point (x y) (method sigma (x) (def y (this --> y)) (x = 1.0) (x + y))
  (method sigma (x) (def y (this --> y)) (x = 1.0) (x + y)))
  (((x Number) (y Number)) 
  ((sigma (Number) Number) (sigma (Number) Number))))

 (import Point)

 (def x 1.0)
 (def point (new Point (x x)))
 (point --> x = x)
 (x = (point --> delta (x)))
 x)
