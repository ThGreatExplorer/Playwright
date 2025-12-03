((tmodule
  BadType
  (class BadType
    (x)
    (method get ()
      (def tmp (this --> x))
      (tmp = (new BadType (tmp)))
      tmp))
  (((x Number)) ((get () Number))))
 (def z 0.0)
 z)