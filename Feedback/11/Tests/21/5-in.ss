((tmodule mOne
          (class goodClassOne (fieldOne))
          (((fieldOne Number)) ()))
 (tmodule mTwo
          (import mOne)
          (class goodClass (fieldOne))
          (((fieldOne (((fieldOne Number)) ()))) ()))
 (import mOne)
 (import mTwo)
 (def x 1.0)
 (def y (new goodClassOne (x)))
 (def z (new goodClass (y)))
 (z isa goodClassOne))