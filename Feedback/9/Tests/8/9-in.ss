(
  (tmodule CoffeeShop
    (class CoffeeMachine ()
      (method brew ()
        (new CoffeeMachine ())))
    ( () ((brew () Number)) ))                

  (tmodule Staff
    (import CoffeeShop)
    (class Barista ()
      (method makeCoffee ()
        (def m (new CoffeeMachine ()))
        (m --> brew ())))
    ( () ((makeCoffee () Number)) ))         

  (import CoffeeShop)
  (import Staff)
  (def alex (new Barista ()))
  (alex --> makeCoffee ())
)
