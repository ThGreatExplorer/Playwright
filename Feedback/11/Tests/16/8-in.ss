(
    (module A (class A ()))
    (tmodule B
        (timport A (() ()))
        (timport A (((a Number)) ()))
        (class B ())
        (() ())
    )
    0.0
)