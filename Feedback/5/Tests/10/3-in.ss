(
    (def x 1.0)
    (def r 0.0)
    (def flag 0.0)
    (while0 flag (block
        (def x 4.0)
        (r = (x + x))
        (flag = 1.0)
    ))
    (x + r)
)