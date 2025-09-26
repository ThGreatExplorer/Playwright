(
    (base = 3.0)
    (power = 10.0)

    (increment = 1.0)
    (outerCounter = 0.0)
    (outerCondition = 0.0)

    (solution = 1.0)

    (while0 outerCondition
        (block
            (if0 (outerCounter == power)
                (outerCondition = 1.0)
                (block
                    (innerCounter = 0.0)
                    (innerCondition = 0.0)
                    (tempSolution = 0.0)
                    (while0 innerCondition
                        (if0 (innerCounter == base)
                            (innerCondition = 1.0)
                            (block
                                (tempSolution = (tempSolution + solution))
                                (innerCounter = (innerCounter + increment))
                            )
                        )
                    )
                    (solution = tempSolution)
                    (outerCounter = (outerCounter + increment))
                )
            )
        )
    )
    solution
)