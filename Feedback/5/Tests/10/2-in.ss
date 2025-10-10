(
    (def x 2.0)
    (def z 0.0)
    (if0 0.0
    (block
        (def x 7.0)
        (z = (x + x))
    )
        (z = z)
    )
    z
)