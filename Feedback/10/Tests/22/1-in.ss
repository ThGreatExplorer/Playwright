((module Point (class Point
                 (x y)
                 (method delta (x) (def y (this --> y)) (x = 1.0) (x + y))))
 (tmodule
  PointTwo
  (timport Point (((x Number) (y Number)) ((delta (Number) Number))))
  (timport
   Point
   (((x Number) (y Number))
    ((delta ((((x Number) (y Number)) ((delta (Number) Number)))) Number))))
  (class Point
    (x y z)
    (method delta () (def x (this --> x)) (def y (this --> y)) (x + y)))
  (((x Number) (y Number))
   ((delta (Number) (((x Number) (y Number)) ((delta (Number) Number)))))))
 (timport Point (((x Number) (y Number)) ((delta (Number) Number))))
 (def x 1.0)
 (def point (new Point (x x)))
 (point --> x = x)
 (x = (point --> delta (x)))
 x)
