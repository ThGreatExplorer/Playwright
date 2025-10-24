(
    (class Dog (speed weight)
        (method setStats ()
            (def s (this --> speed))
            (def w (this --> weight))
            s
        )
    )

    (class Bird (speed weight)
        (method setStats ()
            (def s (this --> speed))
            (def w (this --> weight))
            s
        )
    )

    (def ten 10.0)
    (def twenty 29.0)
    (def myDog (new Dog (ten twenty)))
    (def myBird (new Bird (ten twenty)))
    (def hello 10.0)

    (myDog isa Bird)
)
