(
    (fibOne = 0.0)
    (fibTwo = 1.0)

    (counter = 0.0)
    (increment = 1.0)
    (max = 30.0)
    (conditional = 0.0)

    (while0 conditional
        (block
            (temp = fibTwo)
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