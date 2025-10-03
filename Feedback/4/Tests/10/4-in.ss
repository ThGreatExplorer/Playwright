(
    (def x 0.0)
    (def y 10.0)
    (def negten -10.0)
    (def z 100.0)
    (def h (x + y))
    
    (while0 (y == h)
        (block
            (def test 0.0)
            (z = (z + negten))
            (y = (y / test))
        )
    )
    (y / test)
)