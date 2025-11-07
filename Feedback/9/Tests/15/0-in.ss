((tmodule Point 
   (class Point
     (x y)
     (method sum () 
       (def a (this --> x))
       (def b (this --> y))
       (a + b)))
   (((x Number) (y Number))
    ((sum () Number))))
 (import Point)
 (def x 33.0)
 (def y 34.0)
 (def point (new Point (x y)))
 (point --> sum ()))