(
    (def z 500.0)
    (def y 1.0)
    (def x 500.0)
    (while0 (x == z)
        (block
            (y = 5.0)
            (x = (x + y))
        )
    )
    x
)
