(
    (def x 0.0)
    (def one 1.0)
    (def zero 0.0)
    (if0 0.0
         (block
            (def y 0.0)
            (if0 0.0
                (block
                    (x = (y + one))
                )
                (x = zero)
            )
         )
         (x = zero)
    )
    x
)
