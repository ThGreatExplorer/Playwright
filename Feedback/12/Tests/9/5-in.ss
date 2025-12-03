(
  (module Core
    (class CoreObj (x)
      (method get ()
        (this --> x)
      )
    )
  )

  (tmodule CoreDupe
    (timport Core
      (((x Number)) ((get () Number)))
    )
    (class CoreDupe (v)
      (method run ()
        (def myvalue (this --> v))
        (def o (new CoreObj (myvalue)))
        (o --> get ())
      )
    )
    (((v Number)) ((run () Number)))
  )

  (import CoreDupe)

  (def n 4.0)
  (def cor (new CoreDupe (n)))
  (cor --> run ())
)
