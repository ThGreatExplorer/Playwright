(
  (def x 2.0)
  (def y (x + x))
  (if0 (x == y)
     (block (def z 0.0) (z = z))
     (block (z = (y / x))))
  (while0 (z == x)
    (block (z = (z + y))))
  z
)