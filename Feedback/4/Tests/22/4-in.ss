(
    (def z 500.0)
    (def a 950.0)
    (def x 500.0)
    (def y 0.0)
    (y = (z + a))
    (while0 (x == z)
        (block (def z (z + z)) (def y (y / a)) (x = (x + y))))
    z 
) 