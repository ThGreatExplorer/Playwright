(
    (module a 
        (class A ())
    )
    (tmodule b 
        (timport a (() ()))
        (timport a (() ()))
        (class B ())
        (() ())
    )
    0.1
)