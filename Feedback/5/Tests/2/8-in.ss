((def x 0.0) (def y 0.0) (def ONE 1.0) (def TEN 10.0) (while0 x (block (y = (y + ONE)) (if0 (y == TEN) (x = ONE) (x = x)))) y)
