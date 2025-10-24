(   (class Person (age)
        (method age ()
            (def thirty 30.0)
            (this --> age = thirty)
            (this --> age)
        )
    )

    (def twenty 20.0)
    (def curry (new Person (twenty)))
    (def ans 1.0)

    (if0 ans
        (block
            (def five 5.0)
            (def result (twenty + five))
            (ans = result)
        )
        (block
            (def result (curry --> age ()))
            (ans = result)
        )
    )

    ans
)
