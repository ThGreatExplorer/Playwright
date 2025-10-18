(
  (class C (f)
    (method m ()
      (if0 0.0
           (block (def f 1.0) (f = f))  
           (block (def g 2.0) (g = g))) 
      f                                 
    )
  )
  0.0
)