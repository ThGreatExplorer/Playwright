((def x 1.0) (if0 x (block (def y 2.0) (x = y)) (block (x = 3.0))) (while0 (x / x) (x = (x + x))) x)
