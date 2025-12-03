(
  (module M
    (class Box (v)
      (method get ()
        (this --> v)
      )
      (method set (x)
        (v = x)
      )
    )
  )

  (tmodule BoxTwo
    (timport M
      (((v Number))
       ((get () Number) (set (Number) Number)))
    )
    (class BoxTwo (d)
      (method test ()
        (def b (new Box (d)))
        (def tmp (new Box (d)))    
        (b --> v = tmp)            
        (b --> get ())
      )
    )
    (((d Number))
     ((test () Number)))
  )

  (import BoxTwo)

  (def zz 0.0)
  (def t (new BoxTwo (zz)))
  (t --> test ())
)
