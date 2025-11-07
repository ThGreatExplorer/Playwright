(
  (tmodule Vehicle
    (class Car (module tankAmount)
      (method getModule ()
        (this --> module)))
    (((module Number)
      (tankAmount Number))
     ((getModule () Number))))
  
  (import Vehicle)
  (def fifty 50.0)
  (def zero 0.0)
  (def carOne (new Car (fifty zero)))
  (def carTwo (new Car (zero zero)))

  (if0 zero
       (carOne --> tankAmount = zero)
       (block
         (def zero carTwo)
         (zero = -1.0)
         (carOne --> tankAmount = zero)))

  (carOne --> getModule ())
)
