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
  (carOne --> getModule ())
)
