(
 (module untyped (class C ()))

 (module untypedtwo (import untyped) (class C ()))

 (module untypedthree (import untyped) (import untypedtwo) (class C ()))

 (tmodule typed (class M ()) ( () () ))

 (tmodule typedtwo (import typed) (timport untyped  ( () () )) (class M ()) ( () () ))

 (tmodule typedthree (import typed) (import typedtwo)
          (timport untyped  ( () () ))
          (timport untypedtwo  ( () () ))
          (timport untypedthree  ( () () ))
          (class M ()) ( () () ))

 (import typed)
 (import typedthree)
 (import typedtwo)
 (timport untyped  ( () () ))
 (timport untypedtwo  ( () () ))

 4.0

 )
