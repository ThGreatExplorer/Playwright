((def x 1.0)
 (def y 0.0)
 (if0 0.0
  (block
   (def x 10.0)
   (y = x)
   (if0 0.0
    (block
     (def x 100.0)
     (y = (y + x))
     (if0 0.0
      (block
       (def x 200.0)
       (y = (y + x))
       (if0 0.0
        (block
         (def z 300.0)
         (y = (y + z)))
        (y = y)))
      (y = y)))
    (y = y)))
  (y = y))
 y)