(
  (module Vehicle
    (class Car
      (speed)
      (method accelerate (x)
        (def newSpeed (this --> speed))
        (newSpeed + x))))

  (tmodule VehicleTyped
    (timport Vehicle
      (((speed Number))
       ((accelerate (Number) Number))))

    (class Plane
      (altitude speed)
      (method climb (x)
        (def newAlt (this --> altitude))
        (def newSpeed (this --> speed))
        (newAlt + newSpeed)))

    (((altitude Number) (speed Number))
     ((climb (Number) Number))))

  (import VehicleTyped)

  (timport Vehicle
    (((speed Number))
     ((accelerate (Number) Number))))

  (def oneHundred 100.0)
  (def twoHundred 200.0)
  (def car (new Car (oneHundred)))
  (def plane (new Plane (oneHundred twoHundred)))

  (def resultOne (plane --> climb (oneHundred)))
  (def resultTwo (car --> accelerate (twoHundred)))

  (plane isa Car)

)
