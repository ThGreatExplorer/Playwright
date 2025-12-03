(

 (module empty (class Empty () (method m () 4.0) ))

 (module factory (import empty)
   (class Factory () (method create (o) (def empty (new Empty ())) (o --> field = empty) 4.0)))

 (module two (class ActuallyEmpty ()))

 (tmodule broken
          (class G (field))
          ( ( (field ( () ()))) () ))

 (import broken)
 (timport factory ( () ( (create ( ( ( (field ( () ()))) () ) ) Number)   ) ))
 (timport two ( () () ))
 (def accempty (new ActuallyEmpty ()))
 (def g (new G (accempty)))
 (def factory (new Factory ()))
 (factory --> create (g))
 )


