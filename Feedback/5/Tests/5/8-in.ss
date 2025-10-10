(
(def x 0.0)
(if0 0.0
    (block (def y 1.0) (def x 1.0) (y = (y / x)))
    (x = x))
x)