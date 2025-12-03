(

 (module empty (class Empty (field)))

 (module factory (import empty)
   (class Factory () (method create ()
                             (def four 4.0)
                             (def empty (new Empty (four)))
                             (def outer (new Empty (empty)))
                             outer)))

 (timport factory ( () ( (create ()
                                 ( ((inner  ( ( (inner ( () ())  ) )  () ) )) () )
                                 )   ) ))
 (def factory (new Factory ()))
 (def broken (factory --> create ()))
 4.0
 )


