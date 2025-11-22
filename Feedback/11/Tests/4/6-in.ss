(
 (module untyped (class C ()))

 (tmodule typed (class M ()) ( () () ))

 (tmodule typedtwo (import typed) (class M ()) ( () () ))

 (tmodule typedthree (import typed) (import typedtwo) (class M ()) ( () () ))

 (import typed)
 (import typedthree)
 (import typedtwo)

 4.0

 )
