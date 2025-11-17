((tmodule mOne
          (class goodClass (fieldOne)
            (method module (paramOne) 1.0))
          (((fieldOne Number)) ((module ((() ())) Number))))
 (import mOne)
 (def x 1.0)
 (x = (new goodClass (x)))
 (x --> module (x)))