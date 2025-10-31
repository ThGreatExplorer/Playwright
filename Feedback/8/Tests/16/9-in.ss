((import OldModule)
    (module NewModule 
    (class A (fOne fTwo)
        (method mOne (pOne)
            (def x 1.0)
            (x = pOne)
            x
        )
    ))
 (module OldModule
    (class B (x)
        (method add ()
            (def one 1.0)
            (this --> x = ((this --> x) + one))
            (this --> x))))
(import NewModule)
(import OldModule)
(def five 5.0)
(new B (five)))