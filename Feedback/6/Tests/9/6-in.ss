(
    (class A (x y z)
             (method addition (a b)
                              (b = (a + b))
                              b
             )
    )
    (def objectA (new A ()))
    (def myX (this --> x))
    objectA
)