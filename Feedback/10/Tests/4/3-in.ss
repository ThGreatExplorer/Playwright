(
    (module A
        (class D ()
            (method m () 2.0)))

    (module B
        (import A)
        (class D ()
            (method m () 3.0)
            (method n ()
                (def X (new D ()))
                (X --> m ()))))

    (timport A (() ()))
    (timport B (() ((n () Number))))

    (def d (new D ()))
    (d --> n ())
)