((tmodule
  Counter
  (class Counter
    (v)
    (method pick (x)
      (def tmp 0.0)
      (if0 x
        (block (def z 1.0) (tmp = z))
        (block (def z 2.0) (tmp = z)))
      tmp))
  (
    ( (v Number) )
    ( (pick (Number) Number) )
  ))
 (import Counter)
 (def zero 0.0)
 (def one 1.0)
 (def cond 0.0)
 (def c (new Counter (one)))
 (def r (c --> pick (zero)))
 (while0 cond
   (block
     (cond = one)))
 r)
