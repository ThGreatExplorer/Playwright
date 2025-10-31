(
    (module AModule
        (class A (fOne fTwo)
            (method mOne ()
                (def x 1.0)
                (def pOne 4.0)
                (x = (this --> fOne))
                (this --> fTwo = pOne)
                (this --> fOne)
            )
        )
    )

    (module BModule
        (class A (fThree)
            (method mTwo ()
                (def two 2.0)
                (def pTwo 3.0)
                (def y two)
                (this --> fThree = pTwo)
                (this --> fThree)
            )
        )
    )

    (module CModule
        (class A (fThree)
            (method mTwo ()
                (def ten 10.0)
                (def pTwo 3.0)
                (def y (new A (ten)))
                (this --> fThree = pTwo)
                (this --> fThree)
            )
        )
    )

    (import AModule)
    (import BModule)
    (import CModule)

    
    (def x 100.0)
    (def ten 10.0)
    (def y (new A (ten)))
    (def z 0.0)

    (y isa A)
)


    