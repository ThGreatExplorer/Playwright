((module Helper 
   (class Helper
     (value)
     (method getValue () 
       (this --> value))))
  (tmodule Helper 
   (timport Helper (((value Number)) ((getValue () Number))))
   (class Point
     (x y)
     (method sum () 
       (def a (this --> x))
       (def b (this --> y))
       (a + b)))
   (((x Number) (y Number))
    ((sum () Number))))
 (import Helper)
 (def five 5.0)
 (def helper (new Helper (five)))
 (def x 33.0)
 (def y 34.0)
 (def point (new Helper (x y)))
 (point --> sum ()))
