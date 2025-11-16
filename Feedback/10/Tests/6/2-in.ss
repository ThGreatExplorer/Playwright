(
    (tmodule m
        (class c (fone ftwo)
            (method m (pone ptwo)
                (def fieldone (this --> fone))
                (def fieldtwo (this --> ftwo))
                (if0 (pone == fieldone)
                    (fieldone = ptwo)
                    (if0 (pone == fieldtwo)
                        (fieldtwo = ptwo)
                        (fieldtwo = fieldtwo)
                    )
                )
                0.0
            )
        )
        (
            ((fone Number) (ftwo Number))
            ((m (Number Number) Number))
        )
    )
    (timport m (
                           ((fone Number) (ftwo Number))
                           ((m (Number Number) Number))
                       ))
    (def x 1.0)
    (def y 2.0)
    (new c (x y))
)