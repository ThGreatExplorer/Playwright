(
  (tmodule BOX
    (class Box ())
    (() ()))
  (import BOX)
  (def n 1.0)
  (def b (new Box ()))
  (b + n)
)
