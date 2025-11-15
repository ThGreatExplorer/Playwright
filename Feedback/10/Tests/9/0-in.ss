(                   
  (tmodule A
    (class Point (x y)
      (method getX () (this --> x))
      (method getY () (this --> y)))
    ( ((x Number) (y Number))          
      ((getX () Number) (getY () Number)) ))  

  (import A)
  (def a 10.0)
  (def b 20.0)
  (def p (new Point (a b)))
  (p --> getX ())
)