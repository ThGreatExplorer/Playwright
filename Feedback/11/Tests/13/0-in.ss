((module Person (class Engineer ()))

(tmodule Persontwo (class Doctor
                 (experience age)
                 (method salary () (def a (this --> experience)) (def b (this --> age)) (a + b)))
                 
                 (((experience Number) (age (() ()))) ((salary () Number))))

(timport Person (() ()))
(import Persontwo)
(def exp 20.0)
(def age 59.0)
(def person (new Engineer()))
(def persontwo (new Doctor(exp age)))

(persontwo --> salary()))