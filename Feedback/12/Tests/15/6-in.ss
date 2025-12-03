(
    (tmodule A 
    (class B ())
    (() ())
    )
    (tmodule B (import A)
    (class A ())
    (() ())
    )
    (tmodule C (import A)
    (class ABA () (method yay () (def x (new B ())) x))
    (() ((yay () (() ()))))
    )
    (import A)
    (import C)
    (def x (new B ()))
    (def y (new ABA (x)))
    y
)