(
    (def a 2.0)
    (def b 3.0)
    (def c (a + b))
    (if0 a
        (a = (a + b))
        (block
            (def a b)
            (c = a)
        )
    )
    c
)