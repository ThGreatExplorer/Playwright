((def x 1.0)
 (def y 2.0)
 (def result 0.0)
 (def zero 0.0)
 (if0 zero
      (block (def x 10.0)
             (if0 zero
                  (block (def y 20.0)
                         (result = (x + y)))
                  (result = 0.0)))
      (result = 0.0))
 result)