(
    (x = 1.0)
    (y = 2.0)
    (z = (x + y))
    (a = (y / x))
    (b = (z == a))
    (if0 b
        (block
            (c = (x + z))
            (d = (c / y))
        )
        (block
            (c = (y + z))
            (d = (c / x))
        )
    )
    (while0 z
        (block
            (z = (z / y))
            (if0 z
                (block
                    (m = (a + b))
                    (n = (m == x))
                )
                (block
                    (m = (b + a))
                    (n = (m == y))
                )
            )
        )
    )
    b
)