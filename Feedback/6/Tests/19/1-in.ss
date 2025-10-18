(
 (class PointThreeD
   (x y z)
   (method delta ()
     (def x (this --> x))
     (def y (this --> y))
     (x + y)))
 (def x 1.0)
 (while0 x
   (if0 x
        (x --> y = (new Point (x x)))
        (block
          (def point (new Line (x y z)))
          (x = (x / x)))))
 (point --> x = x)
 (x = (point --> delta (x)))
 x
)
