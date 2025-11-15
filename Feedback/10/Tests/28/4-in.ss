((module
  Base
  (class Base (val)
    (method get () (this --> val))))
 (tmodule
  Wrapper
  (timport Base (((val Number)) ((get () Number))))
  (class Wrapper ()
    (method useBase ()
      (def ten 10.0)
      (def base (new Base (ten)))
      (base --> get ())))
  (()
   ((useBase () Number))))
 (timport Base (((val Number)) ((get () Number))))
 (import Wrapper)
 (def wrapper (new Wrapper ()))
 (wrapper --> useBase ()))