( (module CoffeeShop
    (class CoffeeMachine ()
      (method brew ()
        (new CoffeeMachine ()))))   
  
  (module Staff
    (import CoffeeShop)
    (class Barista ()
      (method makeCoffee ()
        (def m (new CoffeeMachine ()))
        (m --> brew ()))))  
                
  (import CoffeeShop)
  (import Staff)
  (def alex (new Barista ()))
  (alex --> makeCoffee ())
)
