(
    (a = 3.0)
    (c = 7.0)
    (x = 10.0)
    (y = 10.1)
    (abc = 50.0)
    (if0 (x == y)
        (block 
            (z = (a / c))
            (w = (a + c)))
        (block 
            (abc = 100.0)
            (abc = (abc + c)))
    )
    (y + abc)
)