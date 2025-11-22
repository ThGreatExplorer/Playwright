((tmodule
    Vehicle
    (class Car (year tankAmount) (method getYear () (this --> year)))
    (((year Number) (tankAmount Number)) ((getYear () Number))))
   (import Vehicle)
   (def one 1.0)
   (def c (new Car (one one)))
   (one isa Car))