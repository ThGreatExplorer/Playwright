(
    (tmodule M
        (class A ()
            (method m ()
                (def ret 1.0)
                ret
            )
            (method m ()
                (def ret 1.0)
                ret
            )
        )
        (
            ()
            ((m () Number) (m () Number))
        )
    )
    (import M)
    1.0
)