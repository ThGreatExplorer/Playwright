(
    (tmodule A
        (class A ()
            (method f ()
                (def x 1.0)
                x
            )
        )
        (
            ()
            (
                (f () Number)
            )
        )
    )

    (tmodule B
        (import A)
        (class A ()
            (method f ()
                (def x 2.0)
                x
            )
        )
        (
            ()
            (
                (f () Number)
            )
        )
    )
    (tmodule C
        (import B)
        (class C (a)
            (method f (a)
                (def x 2.0)
                x
            )
        )
        (
            (
                (a (

                        ()
                        (
                            (f () Number)
                        )
                    )
                )
            )
            (
                (f (Number) Number)
            )
        )
    )

    (import A)
    (import B)

    (def x 2.0)
    (def a (new A ()))
    (def newX (a --> f ()))
    x
)