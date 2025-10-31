((module Calc
    (class Calc
      (x)
      (method divide (y)
        (x / y))))
 (import Calc)
 (def ten 10.0)
 (def c (new Calc (ten)))
 (def z 0.0)
 (c --> divide (z)))
