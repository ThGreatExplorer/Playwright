((tmodule
  Point
  (class Point (x y) (method delta (x) (def y (this --> y)) 
  (def s 1.0)
  (def t 2.0) 
  (def clas (new Point (s t))) (x = 1.0) (x + y)))
  (((x Number) (y Number)) ((delta (Number) Number))))
  
 (tmodule
  PointTwo
  (import Point)

  (class APoint
    (x y z)
    (method delta (x) (def x (this --> x)) (def y (this --> y))
    
    (def s 1.0)
    (def t 1.0)

    (def pointClass (new Point (s t)))
     pointClass))

  (((x Number) (y Number) (z Number)) 
   ((delta (Number) (((x Number) (y Number)) ((delta (Number) Number)))))))

 (import PointTwo)

 (def x 1.0)
 (def point (new APoint (x x x)))


 point)
