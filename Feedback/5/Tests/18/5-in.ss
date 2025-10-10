( (def a 1.0)
  (def x 2.0)
  (x = 0.0)
  (a = 0.0)
  (while0 x
    (block
      (def a 4.0)
      (def b -1.0)
      (def c 1.0)
      (a = c)
      (x = 1.0)
      (b = (b + a))
      ))
  a
)
