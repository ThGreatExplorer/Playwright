(
  (module Blatant 
    (class Blatant () 
      (method violation () 
        this
      )
    )
  )
  (timport Blatant (() ((violation () Number))))
  (def bla (new Blatant ()))
  (bla --> violation ())
)

