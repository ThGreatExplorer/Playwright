((module AModule
    (class A (fOne fTwo)
        (method mOne (pOne)
            (def x 1.0)
            (x = pOne)
            x
        )
    ))
(module BModule
    (class B (fThree)
        (method mTwo (pTwo)
            (def y 2.0)
            (y = pTwo)
            y
        )
    ))
(module CModule
    (class C (fThree)
        (method mTwo (pTwo)
            (def y 2.0)
            (y = pTwo)
            y
        )
    ))
(import AModule)
(import BModule)
(import CModule)
(def a 10.0)
a)