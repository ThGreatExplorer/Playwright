(
    (x = 0.0) (y = x) 
        (while0 (x == y) (block (if0 x (y = 1.0) (x = 1.0)) (hi = 100.0)))
    (hi / y)
    )