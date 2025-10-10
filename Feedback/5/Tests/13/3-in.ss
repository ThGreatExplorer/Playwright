(
  (def x 5.0)
  (def two 2.0)
  (def one 1.0)
  (if0 (x == x)
       (block 
           (def y one)
           (y = (y + two))
       )
       (block 
           (def y one)
           (y = (y + one))
       )
  )
  y
)