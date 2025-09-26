(
  (x = 2.0)
  (y = (x + x))
  (if0 (x == y)
       (block (z = 0.0))
       (block (z = (y / x))))
  (while0 (z == x)
    (block (z = (z + y))))
  z
)