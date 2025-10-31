(
    (module a 
        (class A ())
    )
    (module b 
        (class B ()
            (method f () (new A ()))
        )
    )
    0.1
)