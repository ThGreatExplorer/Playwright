(
    (tmodule Math
        (class Math ()
            (method exp (base power)
                (def increment 1.0)
                (def counter 0.0)
                (def condition 0.0)

                (def solution 1.0)

                (while0 condition
                    (block
                        (if0 (counter == power)
                            (condition = 1.0)
                            (block
                                (solution = (this --> mult (solution base)))
                                (counter = (counter + increment))
                            )
                        )
                    )
                )
                solution
            )
            (method mult (a b)
                (def result 0.0)
                (def i 0.0)
                (def condition 0.0)
                (def negOne -1.0)
                (def one 1.0)
                (if0 b
                    (result = 0.0)
                    (while0 condition
                        (block
                            (result = (result + a))

                            (i = (i + one))
                            (if0 (i == b)
                                (condition = 1.0)
                                (condition = 0.0)
                            )
                            (i = (i / negOne))
                            (if0 (i == b)
                                (block
                                    (condition = 1.0)
                                    (result = (result / negOne))
                                )
                                (condition = condition)
                            )
                            (i = (i / negOne))
                        )
                    )
                )
                result
            )

            (method sqrt(n)
                (def it 100.0)
                (def result 0.0)
                (if0 n
                    (result = 0.0)
                    (block
                        (def two 2.0)
                        (def guess (n / two))
                        (def i 0.0)
                        (def condition 0.0)
                        (def one 1.0)
                        (while0 condition
                            (block
                                (def guessDivN (n / guess))
                                (def guessSum (guess + guessDivN))
                                (guess = (guessSum / two))

                                (i = (i + one))
                                (if0 (i == it)
                                    (condition = 1.0)
                                    (condition = 0.0)
                                )
                            )
                        )
                        (result = guess)
                    )
                )
                result
            )
        )
        (
            ()
            (
                (exp (Number Number) Number)
                (mult (Number Number) Number)
                (sqrt (Number) Number)
            )
        )
    )

    (tmodule Geo
        (import Math)

        (class pointTwoD (x y)
            (method distanceFromOrigin ()
                (def math (new Math ()))
                (def x (this --> x))
                (def y (this --> y))
                (def two 2.0)
                (def deltaXsq (math --> exp (x two)))
                (def deltaYsq (math --> exp (y two)))
                (def sum (deltaXsq + deltaYsq))
                (math --> sqrt (sum))
            )
        )
        (
            (
                (x Number)
                (y Number)
            )
            (
                (distanceFromOrigin () Number)
            )
        )
    )


    (import Geo)

    (def x 96.0)
    (def y 247.0)
    (def point (new pointTwoD (x y)))
    (def distance (point --> distanceFromOrigin ()))
    distance
)

