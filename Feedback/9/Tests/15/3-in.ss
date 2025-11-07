( 
  (tmodule OWO 
    (class AClass (fave))
    (((fave Number))
     ()))
  
  (tmodule UWU 
    (import OWO)
    (class BClass () 
      (method makeWithFave (f) 
        (new AClass (f)))
      (method updateFave (o newFave)
        (o --> fave = newFave)
        0.0))
    (()
     ((makeWithFave (Number) (((fave Number)) ()))
      (updateFave ((((fave Number)) ()) Number) Number))))
  
  (import UWU)
  (def fave 413.0)
  (def zero 0.0)
  (def anotherFave 612.0)
  (def oOne (new BClass ()))
  (def oTwo (oOne --> makeWithFave(fave)))
  (def status (oOne --> updateFave(oTwo anotherFave)))
  (def result (oTwo --> fave))
  (result / zero)
)