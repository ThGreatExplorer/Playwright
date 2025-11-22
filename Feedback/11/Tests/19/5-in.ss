(
  (tmodule T
    (class Box (x)
      (method get () (this --> x)))
    (((x Number)) ((get () Number))))

  (import T)
  (def n 0.0)
  (def b (new Box (n)))
  (def y (new Box (b)))
  y
)