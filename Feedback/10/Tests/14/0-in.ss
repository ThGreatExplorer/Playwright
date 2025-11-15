(

  (tmodule 
    takeA
  (class change (x))
  (((x (((y Number) (z Number)) ((change () Number) (changeBetter () Number))))) ())
  )

  (module 
    takeB
  (class twoNums (y z) (method changeBetter () 1.0) (method change () 2.0))
  )



  (timport takeB (((y Number) (z Number)) ((changeBetter () Number))))
  (import takeA)

  (def x 1.0)
  (def y 2.0)
  (def a (new twoNums (x y)))
  (def b (new change (a)))
  (b --> x)
)