((module Foo 
          (class foo (x) 
                 (method one () 1.0) ) 
  )
(tmodule Bar 
         (class bar (x y) 
                (method two () 
                        (def yObj (this --> y))
                        (def tmp (yObj --> one ())) 
                        (def double (tmp + tmp))
                        (def thisx (this --> x))
                        (def res (double + thisx))
                        res
                        )
         )
        (
         ((x Number) (y (
                         ((x Number)) ((one () Number))))) 
         ((two () Number))
        )
)
(timport Foo (((x Number)) ((one () Number))))
(import Bar)

(def num 56.0)
(def numTwo 125.0)
(def F (new foo (num)))
(def numTwo (F --> one ()))
(def tmp (new bar (numTwo num)))
(tmp --> two ())
)
