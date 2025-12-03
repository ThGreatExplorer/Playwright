(
  (module U
    (class d ()
      (method makeNum ()
        (new d ())
      )
    )
  )

  (tmodule T
    (timport U (() ((makeNum () Number))))
    (class c ()
      (method m ()
        (def x (new d ()))
        (x --> makeNum ())
      )
    )
    (() ((m () Number)))
  )

  (import T)
  (def obj (new c ()))
  (obj --> m ())
)
