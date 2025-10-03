((def x 1.0)
(def y 2.0)
(if0 x
    (block (def z 3.0)
        (z = (x + z)))
    (block (def z 4.0)
        (z = (y + z))))
z)
