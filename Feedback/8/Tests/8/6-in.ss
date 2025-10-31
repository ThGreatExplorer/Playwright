( (module Data
    (class Box ()
      (method get () 1.0)))
  
  (module Util
    (class Helper ()
      (method make () (new Box ()))))
  
  (module Data                     
    (class Other ()
      (method val () 10.0)))
  
  (import Data)
  (def b (new Box ()))
  (b --> get ())
)
