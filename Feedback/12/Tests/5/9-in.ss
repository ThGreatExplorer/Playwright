(
  (module U
    (class C (f)
      (method getF ()
        (this --> f)
      )
    )
  )

  (tmodule T
    (timport U (((f Number)) ((getF () Number))))
    (class M ()
      (method run ()
        (def num 100.0)
        (def oo (new C (num)))
        (def o (new C (oo)))
        (o --> getF ())
      )
    )
    (() ((run () Number)))
  )

  (import T)
  (def m (new M ()))
  (m --> run ())
)
