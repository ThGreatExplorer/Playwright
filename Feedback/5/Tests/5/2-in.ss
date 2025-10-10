(
    (def x 1.0)
    (if0 x
        (block
            (def y 2.0)
            (x = 5.0))
        (block
            (def y 2.0)
            (def x 1.0)
            (x = 5.0))
    )
    x)
