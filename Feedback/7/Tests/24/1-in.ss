(
  (class Counter (value)
    (method getDouble ()
      (def v (this --> value))
      (v = (v + v))
      v
    )
  )
  (def five 5.0)
  (def ten 10.0)
  (def c (new Counter (five)))
  (c --> value = ten)
  (c --> value)
)