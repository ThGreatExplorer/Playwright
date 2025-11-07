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
(def one 1.0)
(def WRONG 456.0)
(def res 0.0)
(def obj (new foo (one WRONG)))
(if0 obj (res = WRONG) (res = one))
res
)
