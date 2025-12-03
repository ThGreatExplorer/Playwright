((tmodule Bus (class BusDriver()) (() ()))
(module Person (import Bus) (class Driver (age) (method getAge () 5.0)))
(module blatant (import Person) 
            (class Jeep
                (cost)
                (method getDriver (age) (def driver (new Driver(age))) (driver --> getAge()))))
                
(timport blatant (((cost Number)) ((getDriver (Number) Number))))

(def age 22.0)
(def cost 159.0)
(def myJeep (new Jeep (cost)))

(myJeep --> getDriver (age)))