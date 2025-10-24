(
    (class c (f))
    (def x 1.0)
    (def y 2.0)
    (def obj (new c (x)))
    (def objtwo (new c (y)))

    (obj == objtwo)
)