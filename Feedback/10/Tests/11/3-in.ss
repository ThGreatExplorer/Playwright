(
    (tmodule M
        (class A ()
            (method m ()
                (def ret 0.0)
                ret
            )
            (method m ()
                (def ret 0.0)
                ret
            )
        )
        (
            ()
            ((m () Number) (m () Number))
        )
    )
    (import M)
    0.0
)