(
  (module U
    (class Dummy ()
      (method id (x)
        x
      )
    )
  )

  (tmodule T
    (timport U (() ((id (Number) Number))))
    (class M ()
      (method run ()
        (def d (new Dummy ()))
        (d --> id (z))
      )
    )
    (() ((run () Number)))
  )

  (import T)
  (def m (new M ()))
  (m --> run ())
)
