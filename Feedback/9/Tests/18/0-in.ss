((tmodule Vehicle
    (class Car (year tankAmount)
      (method getYear ()
        (this --> year)))
    (((year Number)
      (tankAmount Number))
     ((getYear () Number)
      (getHeight () Number))))
  
  (import Vehicle)
  (def fifty 50.0)
  (def zero 0.0)
  (def carOne (new Car (fifty zero)))
  (carOne --> getYear ()))