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
    
    (def a 10.0)
    a
)