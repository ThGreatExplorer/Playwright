(
    (module a 
        (class A ()
            (method f ()
                4.0
            )
        )
    )
    (timport a (() ()))
    (def var (new A ()))
    0.1
)