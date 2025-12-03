((tmodule
   Source
   (class Source (x) (method getX () (this --> x)))
   (((x Number)) ((getX () Number))))
 (tmodule
   Sink
   (timport Source (((x Number)) ((getX () Number))))
   (class Sink (s) (method extract () (s --> getX ())))
   (((s (((x Number)) ((getX () Number))))) ((extract () Number))))
 (import Sink)
 (def five 5.0)
 (def src (new Source (five)))
 (def snk (new Sink (src)))
 (snk --> extract ()))