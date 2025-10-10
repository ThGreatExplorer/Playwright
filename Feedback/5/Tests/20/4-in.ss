((def x 100.0)
(def y 0.0)
(def z -4.0)
(def a 0.0)
(while0 y
(block (x = (x + z))
(a = (a + x))
(if0 x (y = (y + z))
(y = (y + y)))))
a
)