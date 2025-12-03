(
  (module U
    (class A ()
      (method foo () 0.0)
    )
  )

  (module U
    (class B ()
      (method bar () 1.0)
    )
  )

  (import U)
  (def x 0.0)
  x
)