(
  (tmodule M 
    (class C ()
      (method val () 49.0)
      (method go ()
        (def x (new C ()))
        (x --> val ())
      ))
    ( () ((val () Number) (go () Number)) )   
  )

  (tmodule N 
    (class C ()
      (method val () 39.0)
      (method go ()
        (def x (new C ()))
        (x --> val ())
      ))
    ( () ((val () Number) (go () Number)) )  
  )

  (import M)
  (def y (new C ()))
  (y --> go ())
)