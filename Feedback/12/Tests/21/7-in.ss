((module Point (class Point
                 (x y)
                 (method delta (x) (this --> x))))
 (timport Point (((x Number)
           (y Number))
           ((delta (Number) Number))))
 (def x 1.0)
 (def point (new Point (x x)))
 (if0 point
      (x = 2.0)
      (x = 3.0))
 x)