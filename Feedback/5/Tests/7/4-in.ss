(
    (def c 0.0)
    (if0 c
        (block
            (def a 1.0)
            (a = 1.1)
        )
        (block 
            (def b 2.0)
            (b = a)
        )
    )
    c
)