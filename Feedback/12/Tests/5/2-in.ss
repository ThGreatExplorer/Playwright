(
  (module U
    (class Box (v)
      (method get ()
        (def b 0.0)
        (new Box (b))
      )
    )
  )

  (tmodule T
    (timport U (((v Number)) ((get () Number))))
    (class Main ()
      (method run ()
        (def zero 0.0)
        (def b (new Box (zero)))
        (b --> get ())
      )
    )
    (() ((run () Number)))
  )

  (import T)
  (def m (new Main ()))
  (m --> run ())
)
