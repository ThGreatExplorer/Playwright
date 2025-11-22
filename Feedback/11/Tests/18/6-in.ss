((module Helper 
   (class Helper
     ()
     (method getValue () 
       (this --> value))))
  (tmodule Point 
   (class Point
     (x y)
     (method sum () 
       (def a (this --> x))
       (a + b)))
   (((x Number) (y Number))
    ((sum () Number))))
 (import Point)
 (timport Helper (((value Number)) ((getValue () Number))))
 (def five 5.0)
 (def helper (new Helper (five)))
 (def x 33.0)
 (def y 34.0)
 (def point (new Point (x y)))
 (point --> sum ()))
