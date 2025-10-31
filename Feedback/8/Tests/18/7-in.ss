(
   (module A
   (class Counter
   (count)
   (method getCount () (this --> count))
   (method
    upCount
    ()
    (def crt (this --> count))
    (def one 1.0)
    (this --> count = (crt + one))
    0.0))
   )
   (import A)
 (def u 3.0)
 (def c (new Counter (u)))
 (def d c)
 (u = (d --> upCount ()))
 (c --> getCount ()))