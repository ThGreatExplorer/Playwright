(
    (module A (class A (a)))
    (module B
        (import A)
        (class B (a)
            (method testA ()
                (def a (this --> a))
                (a --> a)
            )
        )
    )

    (timport A (((a Number)) ()))
    (timport B (((a (((a Number)) ()))) ((testA () Number))))
    (def one 1.0)
    (def a (new A (one)))
    (def b (new B (a)))

    (b --> testA ())
)