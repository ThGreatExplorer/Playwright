((tmodule
  Point
  (class Point (x y)
    (method delta (x)
      (def y (this --> y))
      (x = 1.0)
      (x + y)))
  (((x Number) (y Number))
   ((delta (Number) Number)))
 )
 (module
  PointTemp
  (class Point (x y)
    (method delta (x)
      (def y (this --> y))
      (x = 1.0)
      (x + y)))
 )
 (module
  PointTwo
  (timport PointTemp (((x Number) (y Number))
                      ((delta (Number) Number)))
                      )
  (class Point
    (x y z)
    (method delta ()
      (def x (this --> x))
      (def y (this --> y))
      (def obj (new Point (x y)))
      (x + obj)))
 )
 (timport PointTwo (((x Number) (y Number))
                      ((delta (Number) Number))))
 (def x 1.0)
 (def point (new Point (x x)))
 (point --> x = x)
 (x = (point --> delta (x)))
 x)
