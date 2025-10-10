((def x 42.0)
 (def sum 0.0)
 (def y 0.0)
 (while0
  y
  (if0
   (x == y)
   (y = 1.0)
   (block (def z -1.0) (sum = (x + sum)) (x = (x + z)))))
 sum)
