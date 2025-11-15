((tmodule mOne
          (class goodClass (fieldOne) (method m (paramOne) 1.0))
          (((fieldOne Number)) ((m ((() ())) Number))))
 (tmodule mTwo
          (class MTClass ())
          (() ()))
 (import mOne)
 (import mTwo)
 (def a 1.0)
 (def x (new goodClass (a)))
 (def y (new MTClass ()))
 (x --> fieldOne = (new MTClass ()))
 1.0)