((class Point
   (x y)
   (method delta (x) (this --> x))
   (method delta (x) (this --> y)))
 (def x 1.0)
 (def point (new Point (x x)))
 (point --> x = x)
 (x = (point --> delta (x)))
 x)
