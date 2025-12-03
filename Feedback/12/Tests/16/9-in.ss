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

  (((value Number)) 
   ((reset () Number) 
    (inc (Number) Number))))

(tmodule mtwo
  (import testone)
  (class counter (value)
    (method reset ()
      (def zero 0.0)
      (def value zero)
      0.0))

  (((value Number)) 
   ((reset () Number))))


(tmodule mthree
  (import testone)
  (class countertwo (value)
    (method reset ()
      (def zero 0.0)
      (def value zero)
      (new counter (value))))

  (((value Number)) 

   ((reset () 
      (((value Number)) 
      ((reset () Number) 
        (inc (Number) Number)))))))

(timport mthree (()()))


(def five 5.0)
five)