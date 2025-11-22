(
    (module untypedA (class c ()))
    (tmodule typedA
             (timport untypedA (() ()))
             (class c (x)
                      (method return () (this --> x)))
             (((x Number)) ((return () Number)))
    )
    (tmodule typedB
             (import typedA)
             (class add (x)
                        (method adding
                                (a b)
                                (def sum (this --> x))
                                (sum = (a + b))
                                sum)
             )
             (((x Number)) ((adding (Number Number) Number)))
    )
    (import typedB)
    (def one 1.0)
    (def a (new add (one)))
    (a --> adding (one one))
)