(
  (module U
    (class Maker ()
      (method make ()
        5.0
      )
    )
  )

  (tmodule T
    (timport U (() ((make () Number))))
    (class M ()
      (method run ()
        (def mk (new Maker ()))
        (def obj (new M ()))
        (mk --> make (obj))
      )
    )
    (() ((run () Number)))
  )

  (import T)
  (def m (new M ()))
  (m --> run ())
)