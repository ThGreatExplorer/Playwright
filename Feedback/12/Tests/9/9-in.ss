(
  (module Shadow
    (class C (x y)
      (method x () (this --> y))
      (method getY () (this --> y))
    )
  )

  (tmodule TypedShadow
    (timport Shadow
      (((x Number) (y Number))
       ((x () Number) (getY () Number)))
    )
    (class TS (a)
      (method run ()
        (def c (new C (a a)))
        (c --> getY ())
      )
    )
    (((a Number))
     ((run () Number)))
  )

  (import TypedShadow)

  (def z 5.0)
  (def t (new TS (z)))
  (t --> run ())
)
