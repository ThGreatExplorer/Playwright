(
  (module U
    (class Box (v)
      (method get ()
        (def r (this --> v))
        r
      )
    )
  )

  (tmodule T
    (timport U (((v Number)) ((get () Number))))
    (class Main (x)
      (method run ()
        (def y (this --> x))
        (def b (new Box (y)))
        (b --> get ())
      )
    )
    (
      ((x Number))
      ((run () Number))
    )
  )

  (import T)
  (def n 5.0)
  (def c (new Main (n)))
  (c --> run ())
)