(
    (def x 1.0)
    (if0 x (block
        (def y 2.0)
        (def z y)
        (y = z)
    ) (block
        (def y 3.0)
        (x = 7.0)
    ))
    y
)