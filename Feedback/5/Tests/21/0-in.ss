((def x 1.0) (def y 1.0)
    (if0 (y == x)
            (block
                    (def y 3.0)
                     (x = 2.0)
                    (x = (x + y)))
            (x = x))
    x)
