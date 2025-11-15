((tmodule
  Value
  (class Value (n) (method get () (this --> n)))
  (((n Number)) ((get () Number))))
 (tmodule
  Wrapper
  (import Value)
  (class Wrapper (v) (method fetch () (def obj (this --> v)) (obj --> get ())))
  (((v (((n Number)) ((get () Number))))) ((fetch () Number))))
 (import Value)
 (import Wrapper)
 (def x 100.0)
 (def val (new Value (x)))
 (def wrap (new Wrapper (val)))
 (wrap --> fetch ()))
