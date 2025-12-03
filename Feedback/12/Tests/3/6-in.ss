(

 (tmodule broken
          (class G () (method conform (o) 4.0))
          ( () ( (conform  ( ( () () ) )  Number) )   ))

 (module empty (import broken) (class Empty () (method m () (def g (new G ())) (g --> conform (this))) ))

 (timport empty ( () ( (m () Number) ) ))
 (def empty (new Empty ()))
 (empty --> m ())
 )


