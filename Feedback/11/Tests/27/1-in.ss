((module Foo 
          (class foo (x) 
                 (method woo (nOne nTwo) (nOne + nTwo))
                 (method wee () 1.0)
          ) 
  )
(tmodule Bar 
        (timport Foo 
          (
           ((x Number)) 
           ((woo (Number Number) Number) (wee () Number))
          )
        ) 
         (class foo (x y)) 
         (((x Number) (y Number)) ()))
(timport Foo (((x Number)) ((woo (Number Number) Number) (wee () Number)))) 
(import Bar)
(def one 1.0)
(def WRONG 456.0)
(def res 0.0)
(def obj (new foo (one WRONG)))
(if0 obj (res = WRONG) (res = one))
res
)
