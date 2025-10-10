( (def a 1.0)
  (def x 0.0)
  (if0 x
    (block
      (def b 2.0)
      (b = (b + a))
    )
    (block
      (def c 3.0)
      (c = (b + a))
    )
  )
  a
)
