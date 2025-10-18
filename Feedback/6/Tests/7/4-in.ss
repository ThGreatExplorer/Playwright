(
    (class X (a b c d e)
        (method a (b c d)
            (def a 0.0)
            (a = 1.0)
            a

        )
        (method b (b c d)
            (def b 0.0)
            (b = 1.0)
            b
        )
    )
    (class b (a b c d a))
    (def a 1.0)
    (def b 2.0)
    (def Var (new X (a a a a a)))
    (a = b)
    (b = a)

    (Var --> d = (new X (a a a a a)))
    (a = (Var --> c))
    (a = (Var --> a (a b c)))
    (Var isa X)
)