(
  (module OWO
    (class AClass 
      (fave)))
  (tmodule UWU 
    (timport OWO (((wrong Number)) ()))
    (class BClass 
      () 
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
  (def anotherFave 612.0)
  (def oOne (new BClass ()))
  (def oTwo (oOne --> makeWithFave (fave)))
  (def status (oOne --> updateFave (oOne anotherFave)))
  (oTwo --> fave)
)