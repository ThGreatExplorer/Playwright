((module Animals (class Dog
                 (x)
                 (method IncreaseSpeed (x) (x = 7.0) (x / x))))
(module animal (class Dog
                 (x)
                 (method IncreaseSpeed (x) (x = 7.0) (x + x))))

(import Animals)
(import animal)

(def x 5.0)
(def Golden (new Dog (x)))
(def fourteen 14.0)
(Golden --> x = x)

(x = (Golden --> IncreaseSpeed (x)))
(x == fourteen))