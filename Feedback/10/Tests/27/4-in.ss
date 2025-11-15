((tmodule Carnivore
  (class Lion (x y)
    (method babyLion (chicken)
      chicken))
  (((x Number) (y Number)) ((babyLion ((((age Number)) ())) (((age Number)) ())))))
  
(module Omnivore
  (class Chicken (age)))
  
(import Carnivore)
(timport Omnivore (((age Number)) ()))

(def x 5.0)
(def y 5.0)
(def age 1.0)
(def Simba (new Lion (x y)))
(def myChicken (new Chicken (age)))

(myChicken = (Simba --> babyLion (myChicken)))
(myChicken --> age))