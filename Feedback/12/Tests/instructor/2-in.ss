((module Mone (class cone (f) (method s () (this --> f = this) 4.0)))
 (module Mthree (import Mone)
   (class cthree
     (g)
     (method get () (def x (this --> g)) (x --> f))
     (method
      set
      ()
      (def one 1.0)
      (def c (new cone (one)))
      (def x (c --> s ()))
      (this --> g = x)
      666.0)))
 (timport Mone (((f Number)) ((s () Number))))
 (timport
  Mthree
  (((g (((f Number)) ((s () Number))))) ((get () Number) (set () Number))))
 (def x 1.0)
 (def o (new cone (x)))
 (def mone (new cthree (o)))
 (def gone (mone --> set ()))
 (mone --> get ()))
