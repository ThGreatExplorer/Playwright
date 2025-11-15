(
    (module a 
        (class A ())
    )
    (tmodule b 
        (timport a (() ()))
        (timport a (((x Number)) ()))
        (class B ())
        (() ())
    )
    0.1
)