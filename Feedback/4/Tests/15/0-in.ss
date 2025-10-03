( (def a 1.0)
  (def x 2.0)
  (x = 0.0)
  (a = 0.0)
  (while0 x
    (block
      (def a 3.0)
      (def b -1.0)
      (a = 42.0)
      (x = 1.0)
      (b = (b + a))
      ))
  a
)
