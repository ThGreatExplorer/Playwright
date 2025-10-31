(
    (module A 
        (class A ()
            (method get ()
                0.0
            )
        )
    )
    (module B
        (class A ()
            (method get ()
                1.0
            )
        )
    )

    (import B)
    (import A)

    (def A (new A ()))
    (A --> get ())
)