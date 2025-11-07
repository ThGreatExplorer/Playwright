((tmodule
  Point
  (class Point (x y)
    (method sum ()
      (def a (this --> x))
      (def b (this --> y))
      (a + b)))
  (((x Number) (y Number)) ((sum () Number))))
 (import Point)
 (def a 1.0)
 (def p (new Point (a a)))
 (p --> x = (new Point (a a)))
 (p --> sum))