((tmodule Foo 
          (class foo (x) 
                 (method woo (nOne nTwo) (nOne + nTwo))
                 (method wee () 1.0)
          ) 
          (
           ((x Number)) 
           ((woo (Number Number) Number) (wee () Number))
          )
  )
(tmodule Bar (import Foo) 
         (class foo (x y)) 
         (((x Number) (y Number)) ()))

(import Bar)
(def WRONG 456.0)
(def one 1.0)
(def obj (new foo (one WRONG)))
(def obj 200.0)
obj
)
