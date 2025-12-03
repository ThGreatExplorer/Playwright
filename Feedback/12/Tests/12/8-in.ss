((module Hospitol (class Doctor () (method draw () 1.0)))
   (module City (import Hospitol)
     (class Doc
       ()
       (method
        main
        ()
        (def c (new Doctor ()))
        (def x c)
        (x = (x --> draw ()))
        x)))
   (timport City (() ((main () Number))))
   (timport Hospitol (() ((draw () Number))))
   (def obj (new Doc ()))
   (obj --> main ()))