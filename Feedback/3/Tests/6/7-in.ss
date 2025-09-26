((a = 1.0) 
 (b = 2.0) 
 (if0 a 
      (block (c = 3.0) 
             (d = 4.0)) 
      (block (c = 5.0) 
             (if0 a 
                  (d = 6.0) 
                  (d = 7.0)))) 
 (result = (c + d)) 
 result)