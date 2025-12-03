((module
   DataA
   (class DataA (x) (method getX () (this --> x))))
 (module
   DataB
   (class DataB (y) (method getY () (this --> y))))
 (tmodule
   Processor
   (timport DataA (((x Number)) ((getX () Number))))
   (class Processor (d)
     (method process ()
       (def temp (this --> d))
       (temp --> getX ())))
   (((d (((x Number)) ((getX () Number))))) ((process () Number))))
 (import Processor)
 (timport DataA (((x Number)) ((getX () Number))))
 (timport DataB (((y Number)) ((getY () Number))))
 (def five 5.0)
 (def a (new DataA (five)))
 (def p (new Processor (a)))
 (p --> process ()))