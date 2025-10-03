((def x 5.0) 
 (def y 10.0) 
 (def z 0.0) 
 (if0 z 
      (block (x = 20.0)
             (if0 z 
                  (y = x) 
                  (y = 100.0))) 
      (x = 30.0)) 
 (x + y))