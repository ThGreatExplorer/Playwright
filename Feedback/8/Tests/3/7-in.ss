((module ModC
   (class C (x)
     (method test ()
       (def zero 0.0)
       (if0 zero
            (block
              (def newVal 20.0)
              (this --> x = newVal))
            (zero = 0.0))
       (this --> x))))
 (import ModC)
 (def val 5.0)
 (def c (new C (val)))
 (c --> test ()))