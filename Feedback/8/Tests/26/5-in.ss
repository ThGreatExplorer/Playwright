((module Animals (class Dog
                 (x)
                 (method speed (x) (x = 1.0) (x + x))))
 (import Animals)
 (def x 1.0)
 (def doge (new Dog (x)))
 (doge isa Dog))
