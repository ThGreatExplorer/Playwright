((module Animals (class Dog
                 (x y)
                 (method Reducespeed (x) (def y (this --> y)) (x = 7.0) (x / y))))
(module animal (class Dog
                 (x y)
                 (method Reducespeed (x) (def y (this --> y)) (x = 5.0) (x / y))))

(import Animals)
(import animal)

(def x 5.0)
(def y 5.0)
(def Golden (new Dog (x y)))
(Golden --> x = x)
(Golden --> y = y)

(x = (Golden --> Reducespeed (x)))
x)

