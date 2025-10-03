((def x 1.0)
 (if0 0.0
      (block (def x 2.0)
             (if0 0.0
                  (block (def x (x + x))
                         (x = x))
                  (x = x)))
      (x = x))
 x)