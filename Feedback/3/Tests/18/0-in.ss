((x = 0.0)
 (y = 0.0)
 (z = 1.0)
 (while0 (x == y)
   (block 
   (y = (y + z))
   (x = (y + z))))
 (if0 y 
   (hello = (x + y))
   (block (hello = (x + y))
   (hello = (hello + z))))
 (while0 x 
   (x = (x + hello)))
 (if0 y (x = 0.0)
   (block (y = (y + x))
     (x = (x + hello))
     (x = (x + z))
     (y = (y + hello))))
(x == y))
