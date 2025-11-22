(
 (module untyped (class D ()))

 (module untypedtwo (import untyped) (class C () (method m () (def d (new D ())) 4.0)))

 (tmodule typedtwo (timport untypedtwo  ( () () )) (class M ()) ( () () ))

 (import typedtwo)

 4.0

 )
