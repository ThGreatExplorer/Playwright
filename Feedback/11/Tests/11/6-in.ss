(
    (module untypedA (class c ()))
    (tmodule typedA
             (timport untypedA (() ()))
             (class c (x)
                      (method return () (this --> x)))
             (((x Number)) ((return () Number)))
    )
    (import typedA)
    (def one 1.0)
    (def a (new c (one)))
    (a --> return ())
)