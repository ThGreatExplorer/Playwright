((def x 2.0)
 (def result 0.0)
 (def zero 0.0)
 (if0 zero
      (block (def x 0.0)
             (result = (x / x)))
      (result = zero))
 result)