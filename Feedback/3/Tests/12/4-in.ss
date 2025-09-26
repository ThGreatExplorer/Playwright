(
    (x = 0.0)
    (y = 10.0)
    (negten = -10.0)
    (z = 100.0)
    (h = (x + y))
    
    (while0 (y == h)
        (block
            (z = (z + negten))
            (y = (y / x))
        )
    )
    y
)
