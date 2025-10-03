((def x 1.0)
(def y 2.0)
(if0 y 
(block (def z 0.0)
(y = (x + z)))
(block (def z 1.0)
(x = (y + z))))
(x + y))