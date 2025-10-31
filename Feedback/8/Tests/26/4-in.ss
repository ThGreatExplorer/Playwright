((module Animals (class Dog
                 (x y)
                 (method Addspeed (x) (def y (this --> y)) (x = 7.0) (x + y))))
                 
 (import Animals)
 (def x 1.0)
 (def doge (new Dog (x x)))
 (doge --> x = x)
 (x = (doge --> Addspeed (x)))
 x)
