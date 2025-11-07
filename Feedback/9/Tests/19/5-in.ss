(
  (tmodule B
    (class Box (v)
      (method set (x) (this --> v = x) x))
    (((v Number)) ((set (Number) Number))))
  (import B)
  (def vx 1.0)
  (def b (new Box (vx)))
  (b --> set (b))            ; set expects Number, but we pass an object → type error
)
