(
    (def x 100.0)
    (def y x)

    (if0 (x == y)
        (block 
            (def x 10.0) 
            (if0 (x == y) (y = (y + x)) (y = x))
        )
        (y = 0.0)
    )
    y
)