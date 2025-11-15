((tmodule Foo 
          (class foo (x) 
                 (method woo (nOne nTwo) (nOne + nTwo))
          ) 
          (
           ((x Number)) 
           ((woo (Number Number) Number))
          )
  )
(module Foo (import Foo) 
         (class foo (x y)) 
         )
4.0)
