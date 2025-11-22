((module Carnivore
  (class Lion (x y)
    (method babyLion (chicken)
      chicken)))
  
(tmodule Omnivore (timport Carnivore (() ()))
  (class Chicken (age))
  (((age Number)) ()))
  
(timport Carnivore (((x Number) (y Number)) ((babyLion ((((age Number)) ())) (((age Number)) ())))))
(import Omnivore)

(def x 5.0)
(def y 5.0)
(def age 1.0)
(def Simba (new Lion (x y)))
(def myChicken (new Chicken (age)))
(def chickenTwo (Simba --> babyLion (myChicken)))

(myChicken == chickenTwo))