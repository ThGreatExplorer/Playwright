((def x 0.0) (if0 x (block (def y 1.0) (x = 2.0)) (block (def y 2.0) (x = 3.0))) (x + y))
