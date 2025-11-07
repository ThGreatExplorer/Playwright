(
    (tmodule a 
        (class A ()
            (method f () 4.0)
            (method g () 5.0)
        )
        (
            ()
            (
                (g () Number)
                (f () Number)
            )
        )
    )
    (import a)
    0.1
)