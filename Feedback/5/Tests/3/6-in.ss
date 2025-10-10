((def x 5.0)
 (def result 0.0)
 (def zero 0.0)
 (if0 zero
      (block (def x 10.0)
             (result = x))
      (result = 0.0))
 (result = (result + x))
 result)