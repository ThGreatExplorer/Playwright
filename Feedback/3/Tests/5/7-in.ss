((x = 0.0)
 (if0 x
   (block (x = 1.0) (x = (x + x)))
   (block (x = 2.0) (x = (x / x))))
 x)