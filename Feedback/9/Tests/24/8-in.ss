((tmodule testone
  (class counter (value)
    (method reset ()
      (def zero 0.0)
      (def value zero)
      value)
    
    (method inc (amount)
      (def temp (amount + amount))
      (this --> value = temp)
      (this --> value)))
  
  (((value Number)) ((reset () Number) (inc (Number) Number))) )
  
  (import testone)
  
  (def five 5.0)
  (new counter (five)))