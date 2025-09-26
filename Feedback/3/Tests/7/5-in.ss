(
    (a = 1.0)
    (b = 2.0)
    (cond = 0.0)
    (counter = 5.0)
    (decrement = -1.0)
    (while0 cond
        (block
            (a = (a / b))
            (if0 counter
                (cond = (cond + a))
                (counter = (counter + decrement))
            )
        )
    )
    a
)