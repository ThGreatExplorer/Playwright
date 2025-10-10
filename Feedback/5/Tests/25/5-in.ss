(
    (def y 300.0)
    (def x 300.0)
    (def a 0.0)
    (a = 3.0)
    (if0 (x == y)
        (block 
            (def z 500.0)
            (def c -100.0)
            (def w 70.0)
            (a = (y / c))
            (w = (a + c)))
        (block 
            (y = 100.0)
            (x = (x + y)))
    )
    a
)