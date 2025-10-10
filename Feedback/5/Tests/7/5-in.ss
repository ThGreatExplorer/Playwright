(
    (def a 1.0)
    (def b 2.0)
    (def cond 0.0)
    (def counter 5.0)
    (def decrement -1.0)
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