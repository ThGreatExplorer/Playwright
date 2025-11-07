((tmodule mOne
          (class goodClass (fieldOne) (method module (paramOne) 1.0))
          (((fieldOne Number)) ((module ((() ())) Number))))
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