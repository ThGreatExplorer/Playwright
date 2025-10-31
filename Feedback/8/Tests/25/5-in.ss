(
    (module a 
        (class A ()
            (method f () 4.2)
        )
    )
    (module b 
        (import a)
        (class B () 
            (method g () (new A ()))
        )
    )
    (module c 
        (import b)
        (class C ()
            (method h () (new B ()))
        )
    )
    (import c)
    (def c (new C ()))
    (def b (c --> h ()))
    (def a (b --> g ()))
    (a --> f ())
)