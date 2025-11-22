(
 (module untyped (class D ()))

 (module untypedtwo (import untyped) (class C ()
                                       (method m ()
                                               (def d (new D ()))
                                               (def c (d --> field))
                                               (def z (d --> hello ()))
                                               (d --> field = d)

                                               4.0)))

 (tmodule typedtwo (timport untypedtwo  ( () () )) (class M ()) ( () () ))

 (import typedtwo)

 4.0

 )
