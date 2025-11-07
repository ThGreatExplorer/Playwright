((tmodule
   First
   (class Thing (a) (method get () (this --> a)))
   (((a Number)) ((get () Number))))
 (tmodule
   Second
   (import First)
   (class Thing (b) (method getValue () (this --> b)))
   (((b Number)) ((getValue () Number))))
 (import Second)
 (def ninetynine 99.0)
 (def t (new Thing (ninetynine)))
 (t --> getValue ()))