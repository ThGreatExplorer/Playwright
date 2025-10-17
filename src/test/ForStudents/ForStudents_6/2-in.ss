((class PointThreeD
   (x y z)
   (method delta () (def x (this --> x)) (def y (this --> y)) (x + y)))
 (def x 1.0)
 (def point (new Point (x x)))
 (point --> x = x)
 (x = (point --> delta (x)))
 x)
