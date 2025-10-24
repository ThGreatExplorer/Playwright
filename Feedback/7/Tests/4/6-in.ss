((def x 0.0)
 (if0 x
      (block (def y 5.0) (x = y))
      (block (def y 9.0) (x = y)))
 x)
