(
  (class Adder ()
    (method add (a b c) (a + b))
  )
  (def obj (new Adder ()))
  (def x 1.0)
  (def y 2.0)
  (obj --> add (x y))
)