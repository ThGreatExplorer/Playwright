(
 (module untyped (class C ()))

 (tmodule typed (timport untyped ( () () )) (class M ()) ( () () ))

 (import typed)

 4.0

 )
