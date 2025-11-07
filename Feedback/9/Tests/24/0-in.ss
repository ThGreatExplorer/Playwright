((tmodule Animals (class Dog
                 (x)
                 (method IncreaseSpeed (x) (x = 7.0) (x / x)))                 
                 (((x Number)) ((IncreaseSpeed (Number) Number))))

  (tmodule animal (class Cat (x))             
                 (((x Number)) ()))

(import Animals)
(import animal)

(def x 5.0)
(def golden (new Dog (x)))
(def tabby (new Cat (x)))
(golden = tabby)
x)