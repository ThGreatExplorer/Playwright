((module
  timport
  (class module (x y) (method delta (x) (def y (this --> y)) (x = 1.0) (x + y))))
 (timport timport (((x Number) (y Number)) ((delta (Number) Number))))
 (timport timport (((x Number) (y Number)) ((delta (Number) Number))))
 (def x 1.0)
 (def point (new Point (x x)))
 (point --> x = x)
 (x = (point --> delta (x)))
 x)
