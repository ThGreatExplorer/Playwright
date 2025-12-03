(
    (module A (class A (a)))
    (module B
        (import A)
        (class B ()
            (method changeA (a)
                (def b (new B ()))
                (a --> a = b)
                0.0
            )
        )
    )

    (timport A (((a Number)) ()))
    (timport B (
        ()
        ((changeA ((((a Number)) ())) Number))
    ))

    (def one 1.0)
    (def a (new A (one)))
    (def b (new B ()))
    (b --> changeA (a))
)