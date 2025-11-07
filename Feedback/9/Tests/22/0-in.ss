((tmodule
   Cartesian
   (class CartesianPoint (x y)
     (method getX () (this --> x))
     (method getY () (this --> y)))
   (((x Number) (y Number))
    ((getX () Number) (getY () Number))))
 (tmodule
   Polar
   (class PolarPoint (x y)
     (method getX () (this --> x))
     (method getY () (this --> y)))
   (((x Number) (y Number))
    ((getX () Number) (getY () Number))))
 (tmodule
   Computer
   (class Computer ()
     (method computeSum (point)
       (def xval (point --> getX ()))
       (def yval (point --> getY ()))
       (xval + yval)))
   (()
    ((computeSum ((((x Number) (y Number)) ((getX () Number) (getY () Number)))) Number))))
 (import Cartesian)
 (import Polar)
 (import Computer)
 (def three 3.0)
 (def four 4.0)
 (def polar (new PolarPoint (three four)))
 (def comp (new Computer ()))
 (comp --> computeSum (polar)))