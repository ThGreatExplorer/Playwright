((module Foo 
          (class foo (x) ))
 (module Woo (class woo (y)))
 (module Bee (class bee (bzz)))

 (tmodule Bar 
          (timport Foo (((x Number)) ()))
          (timport Woo (((y Number)) ()))
          (class bar (wow))
          (((wow Number)) ()))
 (tmodule All 
          (timport Foo (((x (() ()))) ()))
          (timport Woo (((y Number)) ()))
          (timport Bee (((bzz Number)) ()))
          (class all (damn))
          (((damn Number)) ()))

(timport Foo (((x Number)) ((woo (Number Number) Number) (wee () Number)))) 
(import Bar)
1.0
)
