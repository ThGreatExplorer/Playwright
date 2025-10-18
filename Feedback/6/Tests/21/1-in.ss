(
  (class C (f g)
    (method m (x)
      (def y 1.0)
      (this --> f = x)
      (if0 x
           (block (def z 2.0) (z = 2.0))
           (block (def z 3.0) (z = 3.0)))
      (this --> g)                
    )
  )
  (def c (new C ()))
  (c --> f = 1.0)
  0.0
)
