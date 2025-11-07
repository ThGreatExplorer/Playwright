((tmodule Animals (class Dog
                 (x)
                 (method IncreaseSpeed (x) (x = 7.0) (x / x)))  
                 
                 (((x Number)) ((IncreaseSpeed (Number) Number))))

(tmodule animal (class Dog
                 (x)
                 (method IncreaseSpeed (x) (x = 7.0) 14.0))  
                 
                 (((xm Number)) ((IncreaseSpeed (Number) Number))))

(import Animals)
(import animal)

(def x 5.0)
(def Golden (new Dog (x)))
(def fourteen 14.0)

(x = (Golden --> IncreaseSpeed (x)))
(x == fourteen))