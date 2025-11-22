(
 (module untyped (class C ()))

 (tmodule typed (timport untyped ( () () )) (class M ()) ( () () ))

 (import typed)

 (timport untyped ( ( (field Number) ) () ))

 4.0

 )
