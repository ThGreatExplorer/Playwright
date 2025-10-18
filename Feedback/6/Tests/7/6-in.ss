(
    (class posn (x y)
        (method add ()
            (def x (this --> x))
            (def y (this --> y))
            (x + y)
        )
    )
    (def a 1.0)
    (def point (new posn (a a)))
    (def result (point --> add ()))
    (if0 0.0 (a = 0.0) (a = 0.0))

    result
)