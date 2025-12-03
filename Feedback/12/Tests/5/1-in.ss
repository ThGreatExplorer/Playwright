(
  (module U
    (class writer ()
      (method makeObj ()
        (new writer ())
      )
      (method writeTo (o)
        (o --> x = (this --> makeObj ()))
        0.0
      )
    )
  )

  (tmodule T
    (timport U
      (()
        ((makeObj () Number)
         (writeTo ( (((x Number)) ((run () Number))) ) Number))
      )
    )
    (class S (x)
      (method run ()
        (def w (new writer ()))
        (def zero 0.0)
        (def y (new S (zero)))
        (def t (w --> writeTo (y)))
        (y --> x)
      )
    )
    (((x Number)) ((run () Number)))
  )

  (import T)
  (def zero 0.0)
  (def resObj (new S (zero)))
  (resObj --> run ())
)