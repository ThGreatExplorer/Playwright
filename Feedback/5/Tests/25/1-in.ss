(
    (def x 1000.0)
    (def y 300.0)
    (def h 0.0)
    (h = (h + y))
    (if0 (x == y) 
        (block (h = (x + y)) (def y 20.0))
        (block (def hello 300.0) (y = 100.0)))
    y
)