((def state 0.0) 
 (def acc 0.0) 
 (def one 1.0) 
 (def two 2.0) 
 (def three 3.0) 
 (if0 state 
      (acc = (acc + one)) 
      (acc = (acc + two))) 
 (state = acc) 
 (if0 state 
      (acc = (acc + three)) 
      (acc = (acc + one))) 
 acc)