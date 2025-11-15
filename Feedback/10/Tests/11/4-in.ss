(
    (tmodule M
        (class A ()
            (method m (x x)
                (def ret 0.0)
                ret
            )
        )
        (
            ()
            ((m (Number Number) Number))
        )
    )
    (import M)
    0.0
)