((module NewModule 
    (class A (fOne fTwo)
        (method mOne (pOne)
            (def x 1.0)
            (x = pOne)
            x
        )
    ))
 (module NewModule
    (class B (x)
        (method add ()
            (def one 1.0)
            (this --> x = (one + one))
            (this --> x))))
(import NewModule)
(def five 5.0)
(new B (five)))