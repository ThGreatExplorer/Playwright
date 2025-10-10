((def x 1.0) (if0 x 
    (x = 1.0) 
    (block 
        (def y 2.0)
        (def z 3.0)
        (def a 0.0)
        (while0 a (block 
            (def b 1.0)
            (a = (b + z))
            (x = (a + y)))
        ))
    )
x)