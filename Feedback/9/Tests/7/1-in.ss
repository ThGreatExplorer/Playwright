(
    (tmodule A
        (class A ()
            (method get ()
                0.0
            )
        )
        (
            ()
            ((get () Number))
        )
    )
    (tmodule B
        (class A ()
            (method a ()
                0.0
            )
            (method get ()
                1.0
            )
        )
        (
            ()
            ((get () Number) (get () Number))
        )
    )

    (import B)
    (import A)

    (def A (new A ()))
    (A --> get ())
)