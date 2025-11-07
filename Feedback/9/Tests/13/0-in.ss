((tmodule A (class Accum (sum)
    (method add (inc)
      (def old (this --> sum))
      (this --> sum = (old + inc))
      (this --> sum))
    (method get ()
      (def tmp (this --> sum))
      tmp))
      (((sum Number)) ((add (Number) Number) (get () Number))))

  (tmodule B (class Stepper (count)
    (method down (dec)
      (def cur (this --> count))
      (this --> count = (cur + dec))
      (this --> count)))
      (((count Number)) ((down (Number) Number))))
      
  (tmodule C (class Runner (acc step cap)
    (method loopAdd (unit)
      (def acc (this --> acc))
      (def step (this --> step))
      (def cap (this --> cap))
      (def zero 0.0)
      (def two 2.0)
      (def dec -1.0)
      (def t 0.0)
      (def tmp 0.0)
      (while0 t
        (block
          (def iszero (cap == zero))
          (if0 iszero
            (block
              (t = 1.0))
            (block
              (def flag (cap == two))
              (if0 flag
                (block
                  (tmp = (acc --> add (unit)))
                  (tmp = (acc --> add (unit))))
                (block
                  (tmp = (acc --> add (unit)))))
              (cap = (step --> down (dec)))))))
      (acc --> get ())))
      (((acc (((sum Number)) ((add (Number) Number) (get () Number)))) 
      (step (((count Number)) ((down (Number) Number)))) 
      (cap Number)) 
      ((loopAdd (Number) Number))))
(import A)
(import B)
(import C)
  (def start 0.0)
  (def cap 4.0)
  (def acc (new Accum (start)))
  (def step (new Stepper (cap)))
  (def run (new Runner (acc step cap)))
  (def unit 2.0)
  (def res (run --> loopAdd (unit)))
  res)

