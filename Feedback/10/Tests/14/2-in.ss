((module
  DivideByZero
  (class Divide
    (x y)
    (method div ( ) (def x (this --> x)) (def y (this --> y)) (this --> x = (x + x)) (x / y))))
 (timport DivideByZero (((x Number) (y Number)) ((div () Number))))
 (def ninety 90.0)
 (def zero 0.0)
 (def c (new Divide (ninety zero)))
 (def divResult (c --> div ( )))
 divResult)
