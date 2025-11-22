((module
   Untyped
   (class Untyped (x) (method get () (this --> x))))
 (tmodule
   Typed
   (import Untyped)
   (class Typed (u) (method retrieve () (u --> get ())))
   (((u (((x Number)) ((get () Number))))) ((retrieve () Number))))
 (import Typed)
 (def five 5.0)
 (def u (new Untyped (five)))
 (def t (new Typed (u)))
 (t --> retrieve ()))