(
    (tmodule A
        (class A (a b c d)
            (method get (a b c d)
                0.0
            )
        )
        (
            ((a Number) (b Number) (c Number) (d Number))
            ((get (Number Number Number Number) Number))
        )
    )

    (import A)
    (def w 0.0)
    (def x 1.0)
    (def y 2.0)
    (def z 3.0)
    (def A (new A (w x y z)))
    (A = (new A (z y x w)))
    (A --> get (w x y z))
)