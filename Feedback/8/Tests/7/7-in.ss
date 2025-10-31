(
    (module geo
        (class posn (x y)
            (method f ()
                1.0
            )
        )
    )
    (import geo)
    (def a 1.0)
    (def point (new posn (a a)))
    (a = 2.0)
    point
)