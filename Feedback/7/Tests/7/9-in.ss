(
    (class A ()
        (method f () 
            (def x 1.0)
            x
        )
    )

    (def x 2.0)
    (def a (new A ()))
    (def newX (a --> f ()))
    x
)