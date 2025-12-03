(
    (tmodule M
        (class A ()
            (method m (x x)
                (def ret 1.0)
                ret
            )
        )
        (
            ()
            ((m (Number Number) Number))
        )
    )
    (import M)
    1.0
)