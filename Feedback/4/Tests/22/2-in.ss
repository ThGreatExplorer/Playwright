(
    (def x 100.0)
    (def y 100.0)
    (def h 0.0)
    (while0 (x == y) 
        (block (def y 20.0) (h = (x + y))))
    (y = (h + x))
y)