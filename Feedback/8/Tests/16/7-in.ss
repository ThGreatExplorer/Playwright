((module OldModule 
    (class A (fOne fTwo fOne)
        (method add (pOne)
            (def x 1.0)
            (x = pOne)
            x
        )
    ))
 (module NewModule
    (class B (x)
        (method add (pOne)
            (def zero 0.0)
            (this --> x = (zero + pOne))
            (this --> x))))
(import NewModule)
(def five 5.0)
(new B (five)))