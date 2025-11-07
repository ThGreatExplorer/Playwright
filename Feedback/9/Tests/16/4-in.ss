((tmodule M
   (class Box (x)
     (method getx () (this --> x)))
   (((x Number)) ((getx () Number))))
 (import M)
 (def one 1.5)
 (def b (new Box (one)))
 (b --> getx ()))