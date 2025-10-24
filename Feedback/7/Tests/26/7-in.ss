(
    (class A (fOne fTwo)
        (method mOne (pOne)
            (def one 1.0)
            one
        )
    )

    (def ten 10.0)
    (def twenty 20.0)
    (def firstExample (new A (ten twenty)))
    firstExample
)