(
  (module UOne
    (class BoxOne (v)
      (method get ()
        (def r (this --> v))
        r
      )
    )
  )

  (module UTwo
    (class BoxTwo (w)
      (method getW ()
        (def r (this --> w))
        r
      )
    )
  )

  (tmodule T
    (timport UOne (((v Number)) ((get () Number))))
    (timport UTwo (((w Number)) ((getW () Number))))
    (class Main ()
      (method run ()
        (def a 10.0)
        (def b 32.0)
        (def p (new BoxOne (a)))
        (def q (new BoxTwo (b)))
        (def x (p --> get ()))
        (def y (q --> getW ()))
        (x + y)
      )
    )
    (() ((run () Number)))
  )

  (import T)
  (def m (new Main ()))
  (m --> run ())
)