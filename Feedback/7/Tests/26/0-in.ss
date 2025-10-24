(
  (class counter (value)
    (method reset ()
      (def zero 0.0)
      (def value zero)
      value
    )
    (method inc (amount)
      (def temp (amount + amount))
      (this --> value = temp)
      (this --> value)
    )
  )

  (def five 5.0)
  (def c (new counter (five)))

  (if0 (c --> value)
       (c --> value = 1.0)
       (block
         (while0 (c --> value)
           (block
             (c --> value = (c isa counter))
           )
         )
       )
  )

  (c --> value)
)