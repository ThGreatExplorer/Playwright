((module Point (class Point
                 (x y)
                 (method delta (x)
                         (def newY (new Point (x x)))
                         (this --> y = newY)
                         (this --> x))))
 
 (timport Point (((x Number)
           (y Number))
           ((delta (Number) Number))))
 (def x 1.0)
 (def point (new Point (x x)))
 (def return (point --> delta (x)))
 (point --> y))