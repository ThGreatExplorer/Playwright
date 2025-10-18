(
    (class A (fOne fTwo)
        (method mOne (pOne)
            (def x 1.0)
            (x = pOne)
            x
        )
    )
    (class B (fThree)
        (method mTwo (pTwo)
            (def y 2.0)
            (y = pTwo)
            y
        )
    )
    (class C (fThree)
        (method mTwo (pTwo)
            (def y 2.0)
            (y = pTwo)
            y
        )
    )

    (def x 100.0)
    (def y 200.0)
    (def z 0.0)

    (if0 z
        (block 
            (def result (x + y))
            (z = result)
        )
        (block 
            (def result (y / x))
            (z = result)
        )
    )

    (y isa H)

)