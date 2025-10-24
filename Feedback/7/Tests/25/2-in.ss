(
    (class A (x)
        (method f (n)
            (def result 0.5)
            (def thisx (this --> x))
            (if0 thisx
                (result = n)
                (block
                    (def negOne -1.0)
                    (def one 1.0)
                    (this --> x = (thisx + negOne))
                    (n = (n + one))
                    (result = (this --> f (n)))
                )
            )
            result
        )
    )
    (def num 100.0)
    (def zero 0.0)
    (def a (new A (num)))
    (a --> f (zero))
)