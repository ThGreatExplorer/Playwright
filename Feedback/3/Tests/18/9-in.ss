(
    (x = 0.1)
    (y = 0.2)
    (z = (x + y))
    (k = -0.3)
    (eval = (z + k))
    (if0 eval
    (block (x = 100.0))
    (block (x = 200.0))
    )
    x
)
