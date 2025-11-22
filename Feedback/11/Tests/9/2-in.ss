(
    (tmodule A
        (class A ()
            (method a ()
                0.0
            )
            (method get ()
                0.0
            )
        )
        (
            ()
            ((get () Number) (a () Number))
        )
    )
    (tmodule B
        (class A ()
            (method get ()
                1.0
            )
        )
        (
            ()
            ((get () Number))
        )
    )

    (import B)
    (import A)

    (def A (new A ()))
    (A --> b ())
)