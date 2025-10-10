(
    (def x 1000.0)
    (def y 1000.0)
    (def h 0.0)
    (def w 2.0)
    (if0 (x == y) 
        (block (def x 20.0) (def y 40.0) (h = (x + y)) (y = (y + w)))
        (block (def hello 300.0) (y = 100.0)))
    h
)