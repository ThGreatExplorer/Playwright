((state = 0.0) 
 (acc = 0.0) 
 (one = 1.0) 
 (two = 2.0) 
 (three = 3.0) 
 (if0 state 
      (acc = (acc + one)) 
      (acc = (acc + two))) 
 (state = acc) 
 (if0 state 
      (acc = (acc + three)) 
      (acc = (acc + one))) 
 acc)