(
  (tmodule Vehicle
    (class Car (year)
      (method getYear ()
        (this --> year)))
    (((height Number Number))
     ((getYear Number))))
  
  (import Vehicle)
  (def fifty 50.0)
  (def zero 0.0)
  (def carOne (new Car (fifty zero)))
  (carOne --> getYear ())
)
