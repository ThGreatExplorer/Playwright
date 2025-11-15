(
    (module A
        (class A ()
            (method createA ()
                (new A())
            )
        )
    )
    (timport A (() ((createA () Number))))

    (def a (new A ()))
    (a --> createA ())
)