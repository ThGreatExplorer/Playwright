(
    (class X (a b c d e)
        (method a (b c d)
            (def a 0.0)
            (a = 1.0)
            (c --> a (a a a))

        )
        (method b (b c d)
            (def b 0.0)
            (b = 2.0)
            b
        )
    )
    (class b (a b c d e))
    (def a 1.0)
    (def b (new b (a a a a a)))
    (def Var (new X (a a a a a)))

    (Var --> a (a b a))
)