((module Triangle (class Angles
                       (a b c)
                       (method
                        consume
                        ()
                        (def x (this --> x))
                        (def y (this --> y))
                        (x + y))))
 (import PointThreeD)
 (def x 5.0)
 (def meow (new Meow (x x)))
 (meow --> x = x)
 (x = (meow --> consume (x)))
 x)
