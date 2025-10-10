((def a 1.0) 
 (def b 2.0) 
 (if0 a 
      (block (def c 3.0) 
             (def d 4.0)) 
      (block (def c 5.0) 
             (if0 a 
                  (def d 6.0) 
                  (d = 7.0)))) 
 (def result (c + d)) 
 result)