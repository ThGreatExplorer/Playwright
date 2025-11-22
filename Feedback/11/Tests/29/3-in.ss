((tmodule
   Box
   (class Box (value) (method getValue () (this --> value)))
   (((value Number)) ((getValue () Number))))
 (tmodule
   Container
   (import Box)
   (class Container (b)
     (method unbox ()
       (def temp (this --> b))
       (temp --> getValue ())))
   (((b (((value Number)) ((getValue () Number))))) ((unbox () Number))))
 (import Container)
 (import Box)
 (def ten 10.0)
 (def b (new Box (ten)))
 (def c (new Container (b)))
 (c --> unbox ()))