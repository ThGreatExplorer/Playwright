((module Point (class Point
                 (x y)
                 (method delta (x) (this --> x))))
 (timport Point (((x Number)
           (y Number))
           ((delta (Number) Number))))
 (def x 1.0)
 (def point (new Point (x x)))
 (point isa Point))