(
    (a = 3.0)
    (c = 7.0)
    (x = 10.0)
    (y = 10.1)
    (z = 9.1)
    (if0 (x == y)
        (block 
            (z = (a / c))
            (w = (h + l)))
        (block 
            (abc = (a + c))))
    (abc + z)
)