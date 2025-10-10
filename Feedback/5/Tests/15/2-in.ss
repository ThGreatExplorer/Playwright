((def x 1.0)
 (if0 0.0
   (block (x = 2.0))
   (block (x = 9.0)))
 x)