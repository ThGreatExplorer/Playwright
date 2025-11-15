( (tmodule SquareShape
    (class Square (n)
      (method per () 
       (def x (this --> n))
       (def r (x + x))
       r
      )
      (method area () 
       (def x (this --> n))
       (def d (x + x))
       (def r (d + d))
       r
      )
    )
    (((n Number)) ((per () Number) (area () Number)))
  )
  
  (import SquareShape)
  (def val 4.0)
  (def s (new Square (val)))
  (def a (s --> per ()))
  (def b (s --> area ()))
  (def r (a + b))
  r
)