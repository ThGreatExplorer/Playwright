(
  (module M
    (class C ()
      (method f ()
        (def x 1.0)
        (x = (x + 2.0))
        x
      )
    )
  )
  (import M)
  (def z 1.0)
  (z = 1.0)
  z
)