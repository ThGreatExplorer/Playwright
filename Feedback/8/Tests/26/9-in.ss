((module testone
  (class counter (value)
    (method reset ()
      (def zero 0.0)
      (def value zero)
      value
    )
    (method inc (amount)
      (def temp (amount + amount))
      (this --> value = temp)
      (this --> value)
    )
  ))

  (module mtwo
  (import testone)
  (class counter (value)
    (method reset ()
      (def zero 0.0)
      (def value zero)
      0.0
    )
  ))

  (module mthree
  (import testone)
  (class counter (value)
    (method reset ()
      (def zero 0.0)
      (def value zero)
      (new counter(value))
    )
  ))

(import mthree)

(def five 5.0)
(new counter(five)))