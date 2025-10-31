(
    (module a
        (class A ()
            (method f () 4.0)
        )
    )
    (module b
        (import a)
        (class A ()
            (method f () 5.0)
        )
    )
    (import a)
    (import b)
    (def a (new A ()))
    (a --> f ())
)