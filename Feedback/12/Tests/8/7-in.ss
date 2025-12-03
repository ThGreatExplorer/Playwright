(
    (module A (class A (a b c d)))

    (module B
        (import A)
        (class B ()
            (method getA ()
                (def a 0.0)
                (new A (a a a a))
            )
        )
    )

    (timport B (
        ()
        ((getA () (
            ((a Number) (b Number) (c Number))
            ()
        )))
        )
    )

    (def b (new B ()))
    (def a (b --> getA ()))
    0.0
)