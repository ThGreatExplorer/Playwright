(
    (module BaseMod
        (class A ())
    )

    (tmodule UtilMod
        (timport BaseMod
            ( () () )
        )

        (class Helper (val)
            (method update (x)
                (def classy (new A ()))
                classy
            )
        )

        (
            ((val Number))
            ((update (Number) (() ())))
        )
    )

    (timport BaseMod (()()))
    (import UtilMod)

    (def seed 10.0)
    (def two 2.0)
    (def helperInst (new Helper (seed)))
    (def stored (helperInst --> update (two)))

    (stored isa Helper)
)