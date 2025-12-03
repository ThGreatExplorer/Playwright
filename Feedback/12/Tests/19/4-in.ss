((tmodule
  Point
  (class Point (x y) (method delta (x) (def y (this --> y)) (x = 1.0) (x + y)))
  (((x Number) (y Number)) ((delta (Number) Number))))
  (module
  PointTemp
  (class Point (y)
    (method delta (x)
      (def y (this --> y))
      (def a (x + y))
      (x = 1.0)
      (a + y)))
 )
 (import Point)
 (def x 1.0)
 (def point (new Point (x x)))
 (point --> x = x)
 (x = (point --> delta (x)))
 x)