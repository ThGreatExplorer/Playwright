((module U
  (class U
    (x)
    (method get ()
      (def tmp (this --> x))
      tmp)))
 (tmodule
  BadShapes
  (timport U (((x Number)) ((get () Number))))
  (timport U (((x Number)) ()))
  (class BadShapes
    (dummy)
    (method run ()
      (def d (this --> dummy))
      (def u (new U (d)))
      (u --> get ())))
  (((dummy Number)) ((run () Number))))
 (def n 1.0)
 (def b (new BadShapes (n)))
 (b --> run ()))
