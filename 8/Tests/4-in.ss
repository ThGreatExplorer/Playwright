( 
  (module OWO (class BClass (fave))) 
  (module UWU (import OWO)
    (class AClass () 
      (method makeWithFave (fave) 
        (new BClass (fave)))
      (method updateFave (o fave)
        (o --> fave = fave)
        0.0)))

  (import UWU)
  (import UWU)

  (def fave 413.0)
  (def anotherFave 612.0)
  (def oOne (new AClass ()))
  (def oTwo (oOne --> makeWithFave(fave)))
  (def status (oOne --> updateFave(oTwo anotherFave)))

  (oTwo --> fave)
)