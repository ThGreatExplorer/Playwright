((module interface (class dosomething
                 (x)
                 (method delta (x) (def y (this --> x)) (def fifty 50.0) (fifty + y))))
(module Point (class Point
                 (x y)
                 (method delta (x) (def y (this --> y)) (x = 1.0) (x + y))))
 (module Pointy (import interface) (class Point 
                 (x y z)
                 (method
                  delta
                  ()
                  (def x (this --> x))
                  (def y (this --> y))
                  (def helper (new dosomething (x)))
                  (def result (helper --> delta (x)))
                  result)))
 (import Point)
 (import Pointy)    
 (def x 1.0)
 (def point (new Point (x x x)))
 (point --> x = x)
 (x = (point --> delta ()))
 x)
