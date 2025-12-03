((module Counter
   (class C (n)
     (method inc ()
       (def v (this --> n))
       (def one 1.0)
       (this --> n = (v + one))
       (this --> n))))
 (timport Counter
   (((n Number))
    ((inc () Number))))
 (def zero 0.0)
 (def c (new C (zero)))
 (c --> inc ()))
