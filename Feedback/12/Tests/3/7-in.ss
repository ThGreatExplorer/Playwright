(

 (module accempty (class accEmpty ()))
 (module empty (class Empty (num inner) (method m (param) 4.0) (method n () 4.0)))

 (module factory (import empty) (import accempty)
   (class Factory () (method create ()
                             (def four 4.0)
                             (def inner (new accEmpty()))
                             (def empty (new Empty (four inner)))
                             (def outer (new Empty (four empty)))
                             outer)))

 (timport factory ( () ( (create ()
                                 ( ((inner  ( ( (inner ( () ())  ) (num Number) )  ( (n () Number) (m () Number) ) ) )
                                    (num Number)) ( (n () Number) (m () Number) ) )
                                 )   ) ))
 (def factory (new Factory ()))
 (def broken (factory --> create ()))
 4.0
 )


