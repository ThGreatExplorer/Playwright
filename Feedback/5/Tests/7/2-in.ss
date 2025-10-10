(
    (def fibOne 0.0)
    (def fibTwo 1.0)

    (def counter 0.0)
    (def increment 1.0)
    (def max 30.0)
    (def conditional 0.0)

    (while0 conditional
        (block
            (def temp fibTwo)
            (fibTwo = (fibOne + fibTwo))
            (fibOne = temp)
            (counter = (counter + increment))
            (if0 (max == counter)
                (conditional = 1.0)
                (conditional = 0.0)
            )
        )
    )

    fibOne
)