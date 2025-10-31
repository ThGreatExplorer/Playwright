(
    (module A
        (class A ()
            (method f ()
                (def x 1.0)
                x
            )
        )
    )

    (module B
        (import A)
        (class A ()
            (method f ()
                (def x 2.0)
                x
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