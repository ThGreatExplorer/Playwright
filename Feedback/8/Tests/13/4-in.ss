(
    (module VarX
        (class X (f)
            (method updateX (x)
                (this --> f = x)
                this
            )
        )
    )
    (import VarX)
    (def x 694.0)
    (def bigX (new X (x)))
    (def bigXVal (bigX --> f))
    (if0 0.0 (x = (x / x)) (bigXVal = 0.0))
    (while0 bigXVal (x = (x + x)))
    x
)