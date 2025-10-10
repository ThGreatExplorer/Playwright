((def x 5.0)
 (def zero 0.0)
 (if0 zero
      (block (def x 10.0)
             (x = 20.0))
      (x = 99.0))
 x)