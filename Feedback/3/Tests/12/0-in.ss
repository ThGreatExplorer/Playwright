(
    (x = 100.0)
    (w = 0.0)
    (y = 300.0)
    (me = 100.0)
    (ten = 10.0)
    (z = (x + y))
    (k = 100.0)

    (while0 (x == me)
        (block 
            (y = (y + z)) 
            (if0 (w == k) (x = 0.0) (w = (w + ten)))
        )
    )
    y
)