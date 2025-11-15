(
  (tmodule T
    (class A (x y)
      (method m ()
        (def xlocal x)
        xlocal
      )
    )
    (
      ((x Number) (x Number))
      ((m () Number))
    )
  )
  (import T)
  (def v 1.0)
  v
)