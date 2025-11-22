(
 (module untyped (class C ()))

 (tmodule typed (timport untyped ( () () )) (class M ()) ( () () ))

 (tmodule typedtwo (timport untyped ( ((field Number) ) () )) (class M ()) ( () () ))

 (import typed)

 4.0

 )
