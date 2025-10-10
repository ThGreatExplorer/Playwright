(
    (def x 1.0)
    (def true 0.0)
    (def y 2.0)

    (if0 true
        (block 
            (x = (x + y)))
        (y = x)
    )
    x
)