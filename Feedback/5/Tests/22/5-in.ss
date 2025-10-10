(
(def x 1.0)
(def z 2.0)
(if0 x
	(z = 3.0)
	(block 
		(def z x)
		(x = 0.0)
		(x = (z / x))))
 (z = 0.0)
x
)
