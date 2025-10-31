(
    (module a 
        (class A ()
            (method f () 4.2)
        )
    )
    (module b 
        (import a)
        (class A () 
            (method g () (new A ()))
        )
    )
    (import b)
    (def a (new A ()))
    (a = (a --> g ()))
    (a --> g ())
)