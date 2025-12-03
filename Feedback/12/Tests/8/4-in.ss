(
    (module A
        (class A (a b c d)
            (method get (a b c d)
                0.0
            )
        )
    )

    (module B
        (import A)
        (class B ()
            (method zeroA (a)
                (def z 0.0)
                (a --> a = z)
                (a --> b = z)
                (a --> c = z)
                (a --> d = z)
                (a --> get (z z z z))
            )
        )

    )

    (timport A
        (
            ((a Number) (b Number) (c Number) (d Number))
            ((get (Number Number Number Number) Number))
        )
    )

    (timport B
        (
            ()
            ((zeroA (
                (
                    ((a Number) (b Number) (c Number) (d Number))
                    ((get (Number Number Number Number) Number))
                )
            ) Number))
        )
    )
    (def x 1.0)
    (def a (new A (x x x x)))
    (def b (new B ()))
    (b --> zeroA (a))
)