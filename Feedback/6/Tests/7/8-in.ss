(
    (class Num ()
        (method exp (base power)
            (def increment 1.0)
            (def outerCounter 0.0)
            (def outerCondition 0.0)

            (def solution 1.0)

            (while0 outerCondition
                (block
                    (if0 (outerCounter == power)
                        (outerCondition = 1.0)
                        (block
                            (def innerCounter 0.0)
                            (def innerCondition 0.0)
                            (def tempSolution 0.0)
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
    )
    (def num (new Num ()))
    (def base 3.0)
    (def power 10.0)
    (num --> run (base power))
)