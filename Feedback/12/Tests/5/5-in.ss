(
  (module U
    (class Adder ()
      (method addone (n)
        (def one 1.0)
        (n + one)
      )
    )
  )

  (tmodule T
    (timport U (() ((addone (Number) Number))))
    (class Main ()
      (method run ()
        (def a (new Adder ()))
        (def three 3.0)
        (a --> addone (three))
      )
    )
    (() ((run () Number)))
  )

  (import T)
  (def m (new Main ()))
  (m --> run ())
)
