((def x 1.0)
 (def result 0.0)
 (def zero 0.0)
 (if0 zero
      (block (def nested 2.0)
             (if0 zero
                  (block (def nestedtwo 3.0)
                         (result = nestedtwo))
                  (result = 0.0))
             (result = nestedtwo))
      (result = 0.0))
 result)