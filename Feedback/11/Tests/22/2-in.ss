(
  (tmodule Vehicle
    (class Car (year tankAmount)
      (method getYear ()
        (this --> year))
      (method getTankAmount (year)
        (this --> tankAmount)))
    (((year Number)
      (tankAmount Number))
     ((getYear () Number))))
  
  (import Vehicle)
  (def fifty 50.0)
  (def zero 0.0)
  (def carOne (new Car (fifty zero)))
  carOne
)