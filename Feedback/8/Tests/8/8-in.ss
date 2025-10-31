( (module Math
    (class Calc ()
      (method add (x y) (x + y))
      (method add (x y) (x + y))  
      (method id () 0.0)))
  
  (import Math)
  (def c (new Calc ()))
  (def g 1.0)
  (def h 2.0)
  (c --> add (g h))
)