((module Point (class Point
                 (x y)
                 (method delta (x) (def y (this --> y)) (x = 1.0) (x + y))))
 (module Point (class Point
                 (x y z)
                 (method
                  delta
                  ()
                  (def x (this --> x))
                  (def y (this --> y))
                  (x + y))))
 (import Point)
 (def x 1.0)
 (def point (new Point (x x)))
 (point --> x = x)
 (x = (point --> delta (x)))
 x)
