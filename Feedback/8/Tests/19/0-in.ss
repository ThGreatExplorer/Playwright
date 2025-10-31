((module C
   (class Cowboy ()
     (method draw () 1.0)))

 (module A
   (class Artist ()
     (method draw () 777.0)))

 (import C)
 (def a (new Artist ()))
 (def c (new Cowboy ()))
 (def x 0.0)
 (import A)
 x)
