((x = 1.0)
  (y = (x + x))
  (if0 x
  (block (z = (y / x)) (w = (z == y)))
  (block (z = 0.0)))
  (while0 z
  (block (z = (z / x)) (w = (w == w))))
  w )