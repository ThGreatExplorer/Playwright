((tmodule
  Point
  (class Point (x y) 
    (method x (x) 
      (def y (this --> y))
      (x + y)
    ))
  (((x Number) (y Number) (y Number)) ((x (Number) Number))))
 (import Point)
 (def x 1.0)
 (def point (new Point (x x)))
 (point --> x = x)
 (x = (point --> delta (x)))
 x)