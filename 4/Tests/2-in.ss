(
  (if0 0.0 
    (block
      (x = -1000.0)
      (y = 1000.0)
      (if0 (x == y)
        (block
          (def z 0.0))
        (block
          (x = (x + y))
          (if0 (y / x)
            (x = (x + y))
            (y = (x / y))))))           
    (x = 2.0))
 (x + y))