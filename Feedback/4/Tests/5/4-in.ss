((def counter 1.0)
 (def limit 6.0)
 (def sum 0.0)
 (def inc 1.0)
 (while0 (counter == limit)
   (block
     (sum = (sum + counter))
     (counter = (counter + inc))))
 sum)