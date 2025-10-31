((module mOne
   (class goodClass ()))
 (module mTwo (import mOne)
   (class goodClass ()))
 (import mOne)
 (import mTwo)
 (def x 1.0)
 (x = 2.0)
 (new goodClass ()))