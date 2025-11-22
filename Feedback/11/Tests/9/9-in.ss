(
    (module A
        (class A ()
            (method createA ()
                (new A())
            )
        )
    )
    (tmodule B
        (timport A (() ((createA () Number))))
        (class B ())
        (() ())
    )
    (module C
        (import A)
        (import B)
        (class C ()
            (method createA ()
                (new A())
            )
        )
    )
    (tmodule D
        (timport A (() ()))
        (import B)
        (timport C (() ((createA () Number))))
        (class A ())
        (() ())
    )
    (timport A (() ((createA () Number))))
    (import B)
    (timport C (() ()))
    (import D)

    0.0
)