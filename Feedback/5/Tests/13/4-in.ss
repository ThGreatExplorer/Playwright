(
  (def x 2.0)
  (def y 2.0)
  (while0 (x == y)
    (block
      (def z x)
      (x = (y + x))
      (z = x)
      (if0 (z == y)
           (block 
              (def w x)
              (w = (w + y))
           )
           (block 
              (def w y)
              (x = (w + x))
           )
      )
    )
  )
  x
)