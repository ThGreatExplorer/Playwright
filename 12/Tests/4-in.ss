( 
  (module Mult (class Multiplier ()
    (method times (m n)
      (def negOne -1.0)
      (def result 0.0) 
      (def keepRunning 0.0)

      (while0 keepRunning
        (block 
          (result = (result + m))
          (n = (n + negOne))
          (if0 n
            (keepRunning = 1.0)
            (keepRunning = 0.0))
          ))
      result)))

  (tmodule Fact  
    (class Fact (mult)
      (method calcN (n)
        (def result 1.0)
        (if0 n
          (result = 1.0)
          (block
            (def negOne -1.0)
            (def nMinOne (n + negOne))
            (def calcNminOne (this --> calcN (nMinOne)))

            (def multiplier (this --> mult))
            (result = (multiplier --> times (n calcNminOne)))
          ))
        result))
    (((mult  (() ((times (Number Number) Number)))))
     ((calcN (Number) Number)))
  )

  (import Fact)
  (timport Mult (() ((times (Number Number) Number))))
  
  (def multiplier (new Multiplier ()))
  (def factorial  (new Fact (multiplier)))
  (def n 5.0)
  (factorial --> calcN (n))
)