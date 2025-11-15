(
    (module a 
        (class A ()
            (method f () (new A ()))
        )
    )

    (timport a 
        (() ((f () Number)))
    )

    (def a (new A ()))
    (a --> f ())
)