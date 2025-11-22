(
    (module A 
        (class B () (method getOne () 1.0))
    )
    (timport A (() ()))
    (timport A (() ((getOne () Number))))
    (def x (new B ()))
    (x --> getOne ())
)