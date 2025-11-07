((tmodule
   Animals
   (class Dog
     (x)
     (method speed (x)
       (x = 1.0)
       (x + x)))
   (((x Number))
    ((speed (Number) Number))))
 (import Animals)
 (def x 1.0)
 (def doge (new Dog (x)))
(doge isa Dog))
