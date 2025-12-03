(
    (module importALot (class A ()))
    (tmodule VarX
        (timport importALot
            (
                ((A Number) (B (() ())) (C (((A Number)) ((methodOne () Number))))) 
                ((methodA ((((A Number)) ((methodOne () Number)))) Number) (methodB (Number) Number))
            )
        )
        (timport importALot
            (
                ((A Number) (B (() ())) (C (((A Number)) ((methodOne () Number))))) 
                ((methodA ((((A Number)) ((methodOne () Number)))) Number) (methodB (Number) Number))
            )
        )
        (class X (f)
            (method updateX (x)
                (this --> f)
            )
        )
        (((f Number)) ((updateX (Number) Number)))
    )
    (timport importALot (() ()))
    (import VarX)
    (def x 694.0)
    (def bigX (new X (x)))
    (def bigXVal (bigX --> f))
    (if0 0.0 (x = (x / x)) (bigXVal = 0.0))
    (while0 bigXVal (x = (x + x)))
    x
)