(
    (tmodule A (class A () (method a () 1.0)) (() ((a () Number))))
    (tmodule B
        (import A)
        (class B ()
            (method one () 1.0)
            (method makeA () (new A ()))
        )
        (
            ()
            (
                (makeA () (() ((a () Number))))
                (one () Number)
            )
        )
    )
    (import A)
    (import B)
    (def a (new B ()))
    (def b (a --> makeA ()))
    (def c (b --> a ()))
    (def d (a --> one ()))

    (c + d)
)