((def x 1.0)
 (def res 0.0)
 (def zero 0.0)
 (if0 zero
      (block (def temp 2.0)
             (def sumone (x + temp))
             (if0 zero
                  (block (def temptwo 3.0)
                         (def sumtwo (sumone + temptwo))
                         (if0 zero
                              (block (def tempthree 4.0)
                                     (def sumthree (sumtwo + tempthree))
                                     (res = sumthree))
                              (res = 0.0)))
                  (res = 0.0)))
      (res = 0.0))
 res)