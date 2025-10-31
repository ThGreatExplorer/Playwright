((module Point
    (class Point
      (x y)
      (method delta (x)
        (def y (this --> y))
        (x = 1.0)
        (x + y))))
 (import Point)
 (def x 1.0)
 (def p (new Point (x x)))
 (p --> x = x)
 (x = (p --> delta (x)))
 x)
