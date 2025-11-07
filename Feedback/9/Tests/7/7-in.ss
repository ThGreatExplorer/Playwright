(
    (tmodule geo
        (class posn (x y)
            (method f ()
                1.0
            )
        )

        (((x Number) (y Number)) ((f () Number)))
    )
    (import geo)
    (def a 1.0)
    (def point (new posn (a a)))
    (a = 2.0)
    point
)