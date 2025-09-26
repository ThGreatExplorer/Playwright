(
    (x = 0.0)
    (y = 1.0)
    (z = 0.0)
    (dagger = 0.0)
    (while0 dagger
        (block
        (x = (y + z))
        (y = (z + x))
        (z = (x + y))
        (if0 z
            (block 
                (dagger = (x + y))
                (z = (x + dagger))
                (z = (z + y)))
            (block 
                (dagger = (x + y))
                (z = (x + dagger))
                (z = (z + y)))
                )))
    (z + z))
