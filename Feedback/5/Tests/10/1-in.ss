(
    (def a 10.0)
    (if0 0.0
    (block
        (def a 20.0)
        (if0 0.0
        (block
            (def a 30.0)
            (a = 40.0)
        )
            (a = a)
        )
    )
        (a = a)
    )
    a
)