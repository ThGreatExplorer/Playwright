(
    (module untypedModOther
        (class untypedTwo ()))

    (tmodule typedMod
        (import untypedModOther)
        (class typed ())
        (() ()))

    (import typedMod)
    (def one 1.0)
    (def a (new untyped (one)))
    (a --> x (one))
)