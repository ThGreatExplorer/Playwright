((tmodule
  Counter
  (class Counter (count)
    (method inc ()
      (def one 1.0)
      (def c (this --> count))
      (c + one)))
  (((count Number))
   ((inc () Number))))
 (timport Counter (((count Number)) ((inc () Number))))
 (def zero 0.0)
 (def c (new Counter (zero)))
 (c --> inc ()))