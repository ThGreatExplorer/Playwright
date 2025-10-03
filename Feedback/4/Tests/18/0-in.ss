((def x 1.0)
  (def y (x + x))
  (if0 x
    (block (def z (y / x)) (def w (z == y)) (w = (w + w)))
    (block (def z 0.0) (z = z)))
  (while0 z
    (block (z = (z / x)) (w = (w == w))))
  w )
