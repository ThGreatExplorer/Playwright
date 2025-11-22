(
  (tmodule A
    (class Ca (x)
      (method id (y) y))
    (((x Number)) ((id (Number) Number))))

  (tmodule B
    (import A)
    (class Cb ()
      (method run ()
        (def n 0.0)
        (def a (new Ca (n)))
        (a --> id (n))))
    (() ((run () Number))))

  (import B)
  (def b (new Cb ()))
  (b --> run ())
)
